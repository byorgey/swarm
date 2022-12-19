{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Swarm.Game.Scenario.Objective where

import Control.Lens hiding (from, (<.>))
import Data.Aeson
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Swarm.Game.Scenario.Objective.Logic as L
import Swarm.Language.Pipeline (ProcessedTerm)
import Swarm.TUI.Model.Achievement.Definitions
import Swarm.Util (reflow)

------------------------------------------------------------
-- Scenario objectives
------------------------------------------------------------

data PrerequisiteConfig = PrerequisiteConfig
  { previewable :: Bool
  -- ^ Typically, only the currently "active" objectives are
  -- displayed to the user in the Goals dialog. An objective
  -- is "active" if all of its prerequisites are met.
  --
  -- However, some objectives may be "high-level", in that they may
  -- explain the broader intention behind potentially multiple
  -- prerequisites.
  --
  -- Set this to option True to display this goal in the "upcoming" section even
  -- if the objective has currently unmet prerequisites.
  , logic :: Prerequisite ObjectiveLabel
  -- ^ Boolean expression the represents the condition dependencies which also
  -- must have been evaluated to True.
  -- Note that the achievement of these objective dependencies is
  -- persistent; once achieved, it still counts even if the "condition"
  -- might not still hold. The condition is never re-evaluated once True.
  }
  deriving (Eq, Show, Generic, ToJSON)

instance FromJSON PrerequisiteConfig where
  parseJSON = withObject "prerequisite" $ \v ->
    PrerequisiteConfig
      <$> (v .:? "previewable" .!= False)
      <*> (v .: "logic")

-- | An objective is a condition to be achieved by a player in a
--   scenario.
data Objective = Objective
  { _objectiveGoal :: [Text]
  , _objectiveCondition :: ProcessedTerm
  , _objectiveId :: Maybe ObjectiveLabel
  , _objectiveOptional :: Bool
  , _objectivePrerequisite :: Maybe PrerequisiteConfig
  , _objectiveHidden :: Bool
  , _objectiveAchievement :: Maybe AchievementInfo
  }
  deriving (Show, Generic, ToJSON)

makeLensesWith (lensRules & generateSignatures .~ False) ''Objective

-- | An explanation of the goal of the objective, shown to the player
--   during play.  It is represented as a list of paragraphs.
objectiveGoal :: Lens' Objective [Text]

-- | A winning condition for the objective, expressed as a
--   program of type @cmd bool@.  By default, this program will be
--   run to completion every tick (the usual limits on the number
--   of CESK steps per tick do not apply).
objectiveCondition :: Lens' Objective ProcessedTerm

-- | Optional name by which this objective may be referenced
-- as a prerequisite for other objectives.
objectiveId :: Lens' Objective (Maybe Text)

-- | Indicates whether the objective is not required in order
-- to "win" the scenario. Useful for (potentially hidden) achievements.
-- If the field is not supplied, it defaults to False (i.e. the
-- objective is mandatory to "win").
objectiveOptional :: Lens' Objective Bool

-- | Dependencies upon other objectives
objectivePrerequisite :: Lens' Objective (Maybe PrerequisiteConfig)

-- | Whether the goal is displayed in the UI before completion.
-- The goal will always be revealed after it is completed.
--
-- This attribute often goes along with an Achievement.
objectiveHidden :: Lens' Objective Bool

-- | An optional Achievement that is to be registered globally
-- when this objective is completed.
objectiveAchievement :: Lens' Objective (Maybe AchievementInfo)

instance FromJSON Objective where
  parseJSON = withObject "objective" $ \v ->
    Objective
      <$> (fmap . map) reflow (v .:? "goal" .!= [])
      <*> (v .: "condition")
      <*> (v .:? "id")
      <*> (v .:? "optional" .!= False)
      <*> (v .:? "prerequisite")
      <*> (v .:? "hidden" .!= False)
      <*> (v .:? "achievement")

data CompletionBuckets = CompletionBuckets
  { incomplete :: [Objective]
  , completed :: [Objective]
  , unwinnable :: [Objective]
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data ObjectiveCompletion = ObjectiveCompletion
  { completionBuckets :: CompletionBuckets
  -- ^ This is the authoritative "completion status"
  -- for all objectives.
  -- Note that there is a separate Set to store the
  -- completion status of prerequisite objectives, which
  -- must be carefully kept in sync with this.
  -- Those prerequisite objectives are required to have
  -- labels, but other objectives are not.
  -- Therefore only prerequisites exist in the completion
  -- map keyed by label.
  , completedIDs :: Set.Set ObjectiveLabel
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- | Concatenates all incomplete and completed objectives.
listAllObjectives :: CompletionBuckets -> [Objective]
listAllObjectives (CompletionBuckets x y z) = x <> y <> z

addCompleted :: Objective -> ObjectiveCompletion -> ObjectiveCompletion
addCompleted obj (ObjectiveCompletion buckets cmplIds) =
  ObjectiveCompletion newBuckets newCmplById
 where
  newBuckets =
    buckets
      { completed = obj : completed buckets
      }
  newCmplById = case _objectiveId obj of
    Nothing -> cmplIds
    Just lbl -> Set.insert lbl cmplIds

addUnwinnable :: Objective -> ObjectiveCompletion -> ObjectiveCompletion
addUnwinnable obj (ObjectiveCompletion buckets cmplIds) =
  ObjectiveCompletion newBuckets newCmplById
 where
  newBuckets =
    buckets
      { unwinnable = obj : unwinnable buckets
      }
  newCmplById = cmplIds

setIncomplete ::
  ([Objective] -> [Objective]) ->
  ObjectiveCompletion ->
  ObjectiveCompletion
setIncomplete f (ObjectiveCompletion buckets cmplIds) =
  ObjectiveCompletion newBuckets cmplIds
 where
  newBuckets =
    buckets
      { incomplete = f $ incomplete buckets
      }

addIncomplete :: Objective -> ObjectiveCompletion -> ObjectiveCompletion
addIncomplete obj = setIncomplete (obj :)

-- | Returns the "ObjectiveCompletion" with the "incomplete" goals
-- extracted to a separate tuple member.
-- This is intended as input to a "fold".
extractIncomplete :: ObjectiveCompletion -> (ObjectiveCompletion, [Objective])
extractIncomplete oc =
  (withoutIncomplete, incompleteGoals)
 where
  incompleteGoals = incomplete $ completionBuckets oc
  withoutIncomplete = setIncomplete (const []) oc
