{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | High-level status of scenario play.
-- Representation of progress, logic for updating.
module Swarm.Game.Scenario.Status where

import Control.Lens hiding (from, (<.>))
import Data.Aeson (
  genericParseJSON,
  genericToEncoding,
  genericToJSON,
 )
import Data.Function (on)
import Data.Time (ZonedTime, diffUTCTime, zonedTimeToUTC)
import Data.Yaml as Y
import GHC.Generics (Generic)
import Swarm.Game.CESK (TickNumber)
import Swarm.Game.Scenario
import Swarm.Game.Scenario.Scoring.Best
import Swarm.Game.Scenario.Scoring.CodeSize
import Swarm.Game.Scenario.Scoring.ConcreteMetrics
import Swarm.Game.Scenario.Scoring.GenericMetrics
import Swarm.Game.WorldGen (Seed)
import Swarm.Util.Lens (makeLensesNoSigs)

-- | These launch parameters are used in a number of ways:
-- * Serializing the seed/script path for saves
-- * Holding parse status from form fields, including Error info
-- * Carrying fully-validated launch parameters.
--
-- Type parameters are utilized to support all of these use cases.
data ParameterizableLaunchParams b a = LaunchParms
  { seedVal :: a (Maybe Seed)
  , initialCode :: a (Maybe b)
  }

type SerializableLaunchParms = ParameterizableLaunchParams FilePath Identity
deriving instance Eq SerializableLaunchParms
deriving instance Ord SerializableLaunchParms
deriving instance Show SerializableLaunchParms
deriving instance Read SerializableLaunchParms
deriving instance Generic SerializableLaunchParms
deriving instance FromJSON SerializableLaunchParms
deriving instance ToJSON SerializableLaunchParms

-- | A "ScenarioStatus" stores the status of a scenario along with
--   appropriate metadata: "NotStarted", or "Played".
--   The "Played" status has two sub-states: "Attempted" or "Completed".
data ScenarioStatus
  = NotStarted
  | Played
      SerializableLaunchParms
      -- ^ initial seed and script to run
      ProgressMetric
      BestRecords
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON ScenarioStatus where
  parseJSON = genericParseJSON scenarioOptions

instance ToJSON ScenarioStatus where
  toEncoding = genericToEncoding scenarioOptions
  toJSON = genericToJSON scenarioOptions

getLaunchParams :: ScenarioStatus -> SerializableLaunchParms
getLaunchParams = \case
  NotStarted -> LaunchParms (pure Nothing) (pure Nothing)
  Played x _ _ -> x

-- | A "ScenarioInfo" record stores metadata about a scenario: its
-- canonical path and status.
-- By way of the "ScenarioStatus" record, it stores the
-- most recent status and best-ever status.
data ScenarioInfo = ScenarioInfo
  { _scenarioPath :: FilePath
  , _scenarioStatus :: ScenarioStatus
  }
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON ScenarioInfo where
  parseJSON = genericParseJSON scenarioOptions

instance ToJSON ScenarioInfo where
  toEncoding = genericToEncoding scenarioOptions
  toJSON = genericToJSON scenarioOptions

type ScenarioInfoPair = (Scenario, ScenarioInfo)

makeLensesNoSigs ''ScenarioInfo

-- | The path of the scenario, relative to @data/scenarios@.
scenarioPath :: Lens' ScenarioInfo FilePath

-- | The status of the scenario.
scenarioStatus :: Lens' ScenarioInfo ScenarioStatus

-- | Update the current "ScenarioInfo" record when quitting a game.
--
-- Note that when comparing \"best\" times, shorter is not always better!
-- As long as the scenario is not completed (e.g. some do not have win condition)
-- we consider having fun _longer_ to be better.
updateScenarioInfoOnFinish ::
  CodeSizeDeterminators ->
  ZonedTime ->
  TickNumber ->
  Bool ->
  ScenarioInfo ->
  ScenarioInfo
updateScenarioInfoOnFinish
  csd
  z
  ticks
  completed
  si@(ScenarioInfo p prevPlayState) = case prevPlayState of
    Played initialScript (Metric _ (ProgressStats start _currentPlayMetrics)) prevBestRecords ->
      ScenarioInfo p $
        Played initialScript newPlayMetric $
          updateBest newPlayMetric prevBestRecords
     where
      el = (diffUTCTime `on` zonedTimeToUTC) z start
      cs = codeSizeFromDeterminator csd
      newCompletionFlag = if completed then Completed else Attempted
      newPlayMetric =
        Metric newCompletionFlag $
          ProgressStats start $
            AttemptMetrics (DurationMetrics el ticks) cs
    _ -> si
