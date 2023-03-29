{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Assess pedagogical soundness of the tutorials.
--
-- Approach:
-- 1. Obtain a list of all of the tutorial scenarios, in order
-- 2. Search their "solution" code for `commands`
-- 3. "fold" over the tutorial list, noting which tutorial was first to introduce each command
module Swarm.Doc.Pedagogy (
  renderTutorialProgression,
  generateIntroductionsSequence,
  CoverageInfo (..),
  TutorialInfo (..),
) where

import Control.Arrow ((&&&))
import Control.Lens (universe, view)
import Control.Monad (guard)
import Control.Monad.Except (ExceptT (..), liftIO)
import Data.Char (isLetter)
import Data.List (foldl', sort)
import Data.List.Split (wordsBy)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Game.Entity (loadEntities)
import Swarm.Game.Scenario (Scenario, scenarioDescription, scenarioName, scenarioObjectives, scenarioSolution)
import Swarm.Game.Scenario.Objective (objectiveGoal)
import Swarm.Game.ScenarioInfo (ScenarioCollection, ScenarioInfoPair, flatten, loadScenariosWithWarnings, scenarioCollectionToList, scenarioPath)
import Swarm.Language.Module (Module (..))
import Swarm.Language.Pipeline (ProcessedTerm (..))
import Swarm.Language.Syntax
import Swarm.Language.Types (Polytype)
import Swarm.TUI.Controller (getTutorials)
import Swarm.Util (commaList, simpleErrorHandle)

-- * Constants

wikiPrefix :: Text
wikiPrefix = "https://github.com/swarm-game/swarm/wiki/"

commandsWikiPrefix :: Text
commandsWikiPrefix = wikiPrefix <> "Commands-Cheat-Sheet#"

-- * Types

-- | Tutorials augmented by the set of
-- commands that they introduce.
-- Generated by folding over all of the
-- tutorials in sequence.
data CoverageInfo = CoverageInfo
  { tutInfo :: TutorialInfo
  , novelSolutionCommands :: Set Const
  }

-- | Tutorial scenarios with the set of commands
-- introduced in their solution and descriptions
-- having been extracted
data TutorialInfo = TutorialInfo
  { scenarioPair :: ScenarioInfoPair
  , solutionCommands :: Set Const
  , descriptionCommands :: Set Const
  }

-- | A private type used by the fold
data CommandAccum = CommandAccum
  { _encounteredCmds :: Set Const
  , tuts :: [CoverageInfo]
  }

-- * Functions

-- | Extract commands from both goal descriptions and solution code.
extractCommandUsages :: ScenarioInfoPair -> TutorialInfo
extractCommandUsages siPair@(s, _si) =
  TutorialInfo siPair solnCommands $ getDescCommands s
 where
  solnCommands = S.fromList $ maybe [] getCommands maybeSoln
  maybeSoln = view scenarioSolution s

-- | Obtain the set of all commands mentioned by
-- name in the tutorial's goal descriptions.
--
-- NOTE: It may be more robust to require that a command reference
-- be surrounded by backticks and parse for that accordingly.
getDescCommands :: Scenario -> Set Const
getDescCommands s =
  S.fromList $ mapMaybe (`M.lookup` txtLookups) allWords
 where
  goalTextParagraphs = concatMap (view objectiveGoal) $ view scenarioObjectives s
  allWords = concatMap (wordsBy (not . isLetter) . T.unpack . T.toLower) goalTextParagraphs

  commandConsts = filter isCmd allConst
  txtLookups = M.fromList $ map (T.unpack . syntax . constInfo &&& id) commandConsts

-- | Extract the command names from the source code of the solution.
--
-- NOTE: The processed solution stored in the scenario has been "elaborated";
-- e.g. `noop` gets inserted for an empty `build {}` command.
-- So we explicitly ignore `noop`.
--
-- Also, the code from `run` is not parsed transitively yet.
getCommands :: ProcessedTerm -> [Const]
getCommands (ProcessedTerm (Module stx _) _ _) =
  mapMaybe isCommand nodelist
 where
  ignoredCommands = S.fromList [Run, Noop]

  nodelist :: [Syntax' Polytype]
  nodelist = universe stx
  isCommand (Syntax' _ t _) = case t of
    TConst c -> guard (isCmd c && c `S.notMember` ignoredCommands) >> Just c
    _ -> Nothing

-- | "fold" over the tutorials in sequence to determine which
-- commands are novel to each tutorial's solution.
computeCommandIntroductions :: [ScenarioInfoPair] -> [CoverageInfo]
computeCommandIntroductions =
  reverse . tuts . foldl' f initial
 where
  initial = CommandAccum mempty mempty

  f :: CommandAccum -> ScenarioInfoPair -> CommandAccum
  f (CommandAccum encounteredPreviously xs) siPair =
    CommandAccum updatedEncountered $ CoverageInfo usages novelCommands : xs
   where
    usages = extractCommandUsages siPair
    usedCmdsForTutorial = solutionCommands usages

    updatedEncountered = encounteredPreviously `S.union` usedCmdsForTutorial
    novelCommands = usedCmdsForTutorial `S.difference` encounteredPreviously

-- | Extract the tutorials from the complete scenario collection
-- and derive their command coverage info.
generateIntroductionsSequence :: ScenarioCollection -> [CoverageInfo]
generateIntroductionsSequence =
  computeCommandIntroductions . getTuts
 where
  getTuts =
    concatMap flatten
      . scenarioCollectionToList
      . getTutorials

-- * Rendering functions

-- | Helper for standalone rendering.
-- For unit tests, can instead access the scenarios via the GameState.
loadScenarioCollection :: IO ScenarioCollection
loadScenarioCollection = simpleErrorHandle $ do
  entities <- ExceptT loadEntities
  (_, loadedScenarios) <- liftIO $ loadScenariosWithWarnings entities
  return loadedScenarios

renderUsagesMarkdown :: Int -> CoverageInfo -> Text
renderUsagesMarkdown idx (CoverageInfo (TutorialInfo (s, si) _sCmds dCmds) novelCmds) =
  T.unlines $
    ""
      : firstLine
      : "================"
      : otherLines
 where
  otherLines =
    concat
      [ pure $ "`" <> T.pack (view scenarioPath si) <> "`"
      , [""]
      , pure $ "*" <> T.strip (view scenarioDescription s) <> "*"
      , [""]
      , renderSection "Commands introduced in this solution" $ renderCmds novelCmds
      , [""]
      , renderSection "Commands found in description" $ renderCmds dCmds
      ]

  renderSection title content =
    [title, "----------------"] <> content

  renderCmds cmds =
    pure $
      if null cmds
        then "<none>"
        else commaList . map linkifyCommand . sort . map (T.pack . show) . S.toList $ cmds

  linkifyCommand c = "[" <> c <> "](" <> commandsWikiPrefix <> c <> ")"

  firstLine =
    T.unwords
      [ T.pack $ show idx <> ":"
      , view scenarioName s
      ]

renderTutorialProgression :: IO Text
renderTutorialProgression =
  render . generateIntroductionsSequence <$> loadScenarioCollection
 where
  render = T.unlines . zipWith renderUsagesMarkdown [0 ..]
