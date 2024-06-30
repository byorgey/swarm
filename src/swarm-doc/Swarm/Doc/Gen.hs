{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Auto-generation of various forms of documentation.
module Swarm.Doc.Gen (
  -- ** Main document generation function + types
  generateDocs,
  GenerateDocs (..),
  SheetType (..),

  -- ** Wiki pages
  PageAddress (..),
) where

import Control.Lens (view, (^.))
import Control.Monad (zipWithM, zipWithM_)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import Data.List.Extra (enumerate)
import Data.Map.Lazy (Map, (!))
import Data.Map.Lazy qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Tuple (swap)
import Swarm.Doc.Keyword
import Swarm.Doc.Pedagogy
import Swarm.Doc.Util
import Swarm.Doc.Wiki.Cheatsheet
import Swarm.Game.Entity (Entity, EntityMap (entitiesByName), entityName, entityYields)
import Swarm.Game.Entity qualified as E
import Swarm.Game.Failure (simpleErrorHandle)
import Swarm.Game.Land
import Swarm.Game.Recipe (Recipe, recipeCatalysts, recipeInputs, recipeOutputs)
import Swarm.Game.Robot (Robot, equippedDevices, robotInventory)
import Swarm.Game.Scenario (GameStateInputs (..), ScenarioInputs (..), loadStandaloneScenario, scenarioLandscape)
import Swarm.Game.World.Gen (extractEntities)
import Swarm.Game.World.Typecheck (Some (..), TTerm)
import Swarm.Language.Key (specialKeyNames)
import Swarm.Util (both)
import Text.Dot (Dot, NodeId, (.->.))
import Text.Dot qualified as Dot

-- ============================================================================
-- MAIN ENTRYPOINT TO CLI DOCUMENTATION GENERATOR
-- ============================================================================
--
-- These are the exported functions used by the executable.
--
-- ----------------------------------------------------------------------------

-- | An enumeration of the kinds of documentation we can generate.
data GenerateDocs where
  -- | Entity dependencies by recipes.
  RecipeGraph :: GenerateDocs
  -- | Keyword lists for editors.
  EditorKeywords :: Maybe EditorType -> GenerateDocs
  -- | List of special key names recognized by 'Swarm.Language.Syntax.Key' command
  SpecialKeyNames :: GenerateDocs
  -- | Cheat sheets for inclusion on the Swarm wiki.
  CheatSheet :: PageAddress -> Maybe SheetType -> GenerateDocs
  -- | List command introductions by tutorial
  TutorialCoverage :: GenerateDocs
  deriving (Eq, Show)

-- | Generate the requested kind of documentation to stdout.
generateDocs :: GenerateDocs -> IO ()
generateDocs = \case
  RecipeGraph -> generateRecipe >>= putStrLn
  EditorKeywords e ->
    case e of
      Just et -> generateEditorKeywords et
      Nothing -> do
        putStrLn "All editor completions:"
        let editorGen et = do
              putStrLn $ replicate 40 '-'
              putStrLn $ "-- " <> show et
              putStrLn $ replicate 40 '-'
              generateEditorKeywords et
        mapM_ editorGen enumerate
  SpecialKeyNames -> generateSpecialKeyNames
  CheatSheet address s -> makeWikiPage address s
  TutorialCoverage -> renderTutorialProgression >>= putStrLn . T.unpack

-- ----------------------------------------------------------------------------
-- GENERATE KEYWORDS: LIST OF WORDS TO BE HIGHLIGHTED
-- ----------------------------------------------------------------------------

-- | Generate a list of keywords in the format expected by one of the
--   supported editors.
generateEditorKeywords :: EditorType -> IO ()
generateEditorKeywords = \case
  Emacs -> do
    putStrLn "(defvar swarm-mode-builtins '("
    T.putStr $ builtinFunctionList Emacs <> "))"
    putStrLn "\n(defvar swarm-mode-commands '("
    T.putStr $ keywordsCommands Emacs
    T.putStr $ keywordsDirections Emacs <> "))"
    putStrLn "\n (defvar swarm-mode-operators '("
    T.putStr $ operatorNames Emacs <> "))"
  VSCode -> do
    putStrLn "Functions and commands:"
    T.putStrLn $ builtinFunctionList VSCode <> "|" <> keywordsCommands VSCode
    putStrLn "\nDirections:"
    T.putStrLn $ keywordsDirections VSCode
    putStrLn "\nOperators:"
    T.putStrLn $ operatorNames VSCode
  Vim -> do
    putStrLn "syn keyword Builtins "
    T.putStr $ builtinFunctionList Vim
    putStrLn "\nsyn keyword Command "
    T.putStr $ keywordsCommands Vim
    putStrLn "\nsyn keyword Direction "
    T.putStr $ keywordsDirections Vim
    putStrLn "\nsyn match Operators "
    T.putStr $ "[" <> operatorNames Vim <> "]"

-- ----------------------------------------------------------------------------
-- GENERATE SPECIAL KEY NAMES
-- ----------------------------------------------------------------------------

generateSpecialKeyNames :: IO ()
generateSpecialKeyNames =
  T.putStr . T.unlines . Set.toList $ specialKeyNames

-- ----------------------------------------------------------------------------
-- GENERATE GRAPHVIZ: ENTITY DEPENDENCIES BY RECIPES
-- ----------------------------------------------------------------------------

generateRecipe :: IO String
generateRecipe = simpleErrorHandle $ do
  (classic, GameStateInputs (ScenarioInputs worlds (TerrainEntityMaps _ entities)) recipes) <- loadStandaloneScenario "data/scenarios/classic.yaml"
  baseRobot <- instantiateBaseRobot $ classic ^. scenarioLandscape
  return . Dot.showDot $ recipesToDot baseRobot (worlds ! "classic") entities recipes

recipesToDot :: Robot -> Some (TTerm '[]) -> EntityMap -> [Recipe Entity] -> Dot ()
recipesToDot baseRobot classicTerm emap recipes = do
  Dot.attribute ("rankdir", "LR")
  Dot.attribute ("ranksep", "2")
  world <- diamond "World"
  base <- diamond "Base"
  -- --------------------------------------------------------------------------
  -- add nodes with for all the known entities
  let enames' = toList . Map.keysSet . entitiesByName $ emap
      enames = filter (`Set.notMember` ignoredEntities) enames'
  ebmap <- Map.fromList . zip enames <$> mapM (box . unpack) enames
  -- --------------------------------------------------------------------------
  -- getters for the NodeId based on entity name or the whole entity
  let safeGetEntity m e = fromMaybe (error $ unpack e <> " is not an entity!?") $ m Map.!? e
      getE = safeGetEntity ebmap
      nid = getE . view entityName
  -- --------------------------------------------------------------------------
  -- Get the starting inventories, entities present in the world and compute
  -- how hard each entity is to get - see 'recipeLevels'.
  let devs = startingDevices baseRobot
      inv = startingInventory baseRobot
      worldEntities = case classicTerm of Some _ t -> extractEntities t
      levels = recipeLevels recipes (Set.unions [worldEntities, devs])
  -- --------------------------------------------------------------------------
  -- Base inventory
  (_bc, ()) <- Dot.cluster $ do
    Dot.attribute ("style", "filled")
    Dot.attribute ("color", "lightgrey")
    mapM_ ((base ---<>) . nid) devs
    mapM_ ((base .->.) . nid . fst) $ Map.toList inv
  -- --------------------------------------------------------------------------
  -- World entities
  (_wc, ()) <- Dot.cluster $ do
    Dot.attribute ("style", "filled")
    Dot.attribute ("color", "forestgreen")
    mapM_ (uncurry (Dot..->.) . (world,) . getE . view entityName) (toList worldEntities)
  -- --------------------------------------------------------------------------
  let -- put a hidden node above and below entities and connect them by hidden edges
      wrapBelowAbove :: Set Entity -> Dot (NodeId, NodeId)
      wrapBelowAbove ns = do
        b <- hiddenNode
        t <- hiddenNode
        let ns' = map nid $ toList ns
        mapM_ (b .~>.) ns'
        mapM_ (.~>. t) ns'
        return (b, t)
      -- put set of entities in nice
      subLevel :: Int -> Set Entity -> Dot (NodeId, NodeId)
      subLevel i ns = fmap snd . Dot.cluster $ do
        Dot.attribute ("style", "filled")
        Dot.attribute ("color", "khaki")
        bt <- wrapBelowAbove ns
        Dot.attribute ("rank", "sink")
        -- the normal label for cluster would be cover by lines
        _bigLabel <-
          Dot.node
            [ ("shape", "plain")
            , ("label", "Bottom Label")
            , ("fontsize", "20pt")
            , ("label", "Level #" <> show i)
            ]
        return bt
  -- --------------------------------------------------------------------------
  -- order entities into clusters based on how "far" they are from
  -- what is available at the start - see 'recipeLevels'.
  bottom <- wrapBelowAbove worldEntities
  ls <- zipWithM subLevel [1 ..] (drop 1 levels)
  let invisibleLine = zipWithM_ (.~>.)
  tls <- mapM (const hiddenNode) levels
  bls <- mapM (const hiddenNode) levels
  invisibleLine tls bls
  invisibleLine bls (drop 1 tls)
  let sameBelowAbove (b1, t1) (b2, t2) = Dot.same [b1, b2] >> Dot.same [t1, t2]
  zipWithM_ sameBelowAbove (bottom : ls) (zip bls tls)
  -- --------------------------------------------------------------------------
  -- add node for the world and draw a line to each entity found in the wild
  -- finally draw recipes
  let recipeInOut r = [(snd i, snd o) | i <- r ^. recipeInputs, o <- r ^. recipeOutputs]
      recipeReqOut r = [(snd q, snd o) | q <- r ^. recipeCatalysts, o <- r ^. recipeOutputs]
      recipesToPairs f rs = both nid <$> nubOrd (concatMap f rs)
  mapM_ (uncurry (.->.)) (recipesToPairs recipeInOut recipes)
  mapM_ (uncurry (---<>)) (recipesToPairs recipeReqOut recipes)
  -- --------------------------------------------------------------------------
  -- also draw an edge for each entity that "yields" another entity
  let yieldPairs = mapMaybe (\e -> (e ^. entityName,) <$> (e ^. entityYields)) . Map.elems $ entitiesByName emap
  mapM_ (uncurry (.->.)) (both getE <$> yieldPairs)

-- ----------------------------------------------------------------------------
-- RECIPE LEVELS
-- ----------------------------------------------------------------------------

-- | Order entities in sets depending on how soon it is possible to obtain them.
--
-- So:
--  * Level 0 - starting entities (for example those obtainable in the world)
--  * Level N+1 - everything possible to make (or drill) from Level N
--
-- This is almost a BFS, but the requirement is that the set of entities
-- required for recipe is subset of the entities known in Level N.
--
-- If we ever depend on some graph library, this could be rewritten
-- as some BFS-like algorithm with added recipe nodes, but you would
-- need to enforce the condition that recipes need ALL incoming edges.
recipeLevels :: [Recipe Entity] -> Set Entity -> [Set Entity]
recipeLevels recipes start = levels
 where
  recipeParts r = ((r ^. recipeInputs) <> (r ^. recipeCatalysts), r ^. recipeOutputs)
  m :: [(Set Entity, Set Entity)]
  m = map (both (Set.fromList . map snd) . recipeParts) recipes
  levels :: [Set Entity]
  levels = reverse $ go [start] start
   where
    isKnown known (i, _o) = null $ i Set.\\ known
    nextLevel known = Set.unions . map snd $ filter (isKnown known) m
    go ls known =
      let n = nextLevel known Set.\\ known
       in if null n
            then ls
            else go (n : ls) (Set.union n known)

startingDevices :: Robot -> Set Entity
startingDevices = Set.fromList . map snd . E.elems . view equippedDevices

startingInventory :: Robot -> Map Entity Int
startingInventory = Map.fromList . map swap . E.elems . view robotInventory

-- | Ignore utility entities that are just used for tutorials and challenges.
ignoredEntities :: Set Text
ignoredEntities =
  Set.fromList
    [ "upper left corner"
    , "upper right corner"
    , "lower left corner"
    , "lower right corner"
    , "horizontal wall"
    , "vertical wall"
    ]

-- ----------------------------------------------------------------------------
-- GRAPHVIZ HELPERS
-- ----------------------------------------------------------------------------

customNode :: [(String, String)] -> String -> Dot NodeId
customNode attrs label = Dot.node $ [("style", "filled"), ("label", label)] <> attrs

box, diamond :: String -> Dot NodeId
box = customNode [("shape", "box")]
diamond = customNode [("shape", "diamond")]

-- | Hidden node - used for layout.
hiddenNode :: Dot NodeId
hiddenNode = Dot.node [("style", "invis")]

-- | Hidden edge - used for layout.
(.~>.) :: NodeId -> NodeId -> Dot ()
i .~>. j = Dot.edge i j [("style", "invis")]

-- | Edge for recipe requirements and outputs.
(---<>) :: NodeId -> NodeId -> Dot ()
e1 ---<> e2 = Dot.edge e1 e2 attrs
 where
  attrs = [("arrowhead", "diamond"), ("color", "blue")]
