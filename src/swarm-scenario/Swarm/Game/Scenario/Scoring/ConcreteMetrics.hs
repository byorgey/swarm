{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Data types and instances for specific scoring methods
module Swarm.Game.Scenario.Scoring.ConcreteMetrics where

import Control.Lens hiding (from, (<.>))
import Data.Aeson
import Data.Char (toLower)
import Data.Time (NominalDiffTime)
import GHC.Generics (Generic)
import Swarm.Game.Scenario.Scoring.CodeSize
import Swarm.Game.Tick (TickNumber (..))

scenarioOptions :: Options
scenarioOptions =
  defaultOptions
    { fieldLabelModifier = map toLower . drop (length ("_scenario" :: String))
    }

data DurationMetrics = DurationMetrics
  { _scenarioElapsed :: NominalDiffTime
  -- ^ Time elapsed until winning the scenario.
  , _scenarioElapsedTicks :: TickNumber
  -- ^ Ticks elapsed until winning the scenario.
  }
  deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''DurationMetrics

emptyDurationMetric :: DurationMetrics
emptyDurationMetric = DurationMetrics 0 $ TickNumber 0

instance FromJSON DurationMetrics where
  parseJSON = genericParseJSON scenarioOptions

instance ToJSON DurationMetrics where
  toEncoding = genericToEncoding scenarioOptions
  toJSON = genericToJSON scenarioOptions

data AttemptMetrics = AttemptMetrics
  { _scenarioDurationMetrics :: DurationMetrics
  , _scenarioCodeMetrics :: Maybe ScenarioCodeMetrics
  -- ^ Size of the user's program.
  }
  deriving (Eq, Ord, Show, Read, Generic)

emptyAttemptMetric :: AttemptMetrics
emptyAttemptMetric = AttemptMetrics emptyDurationMetric Nothing

makeLenses ''AttemptMetrics

instance FromJSON AttemptMetrics where
  parseJSON = genericParseJSON scenarioOptions

instance ToJSON AttemptMetrics where
  toEncoding = genericToEncoding scenarioOptions
  toJSON = genericToJSON scenarioOptions
