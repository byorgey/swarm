{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- XXX move this to Swarm.Log, update documentation to make it more
-- generic --- used both in robots as well as in RuntimeState.eventLog
--
-- XXX or make a separate 'SystemLogEntry' type, as suggested by the
-- TODO below?
--
-- XXX Also make 'Notification' documentation more generic
--
-- XXX eventLog uses negative numbers for robot ID?  Make robot ID
-- optional perhaps?
--
-- A data type to represent in-game logs by robots.
--
-- Because of the use of system robots, we sometimes
-- want to use special kinds of logs that will be
-- shown to the player.
--
-- TODO: #1039 Currently we abuse this system for system
-- logs, which is fun, but we should eventually make
-- a dedicated `SystemLogEntry` type for 'RuntimeState'
-- message queue.
module Swarm.Game.Log (
  Severity (..),
  RobotLogSource (..),
  LogSource (..),
  LogLocation (..),
  LogEntry (..),
  leTime,
  leSource,
  leSeverity,
  leName,
  leLocation,
  leText,
) where

import Control.Lens hiding (contains)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Swarm.Game.CESK (TickNumber)
import Swarm.Game.Location (Location)
import Swarm.Game.Universe (Cosmic)

-- | Severity of the error - critical errors are bugs
--   and should be reported as Issues.
data Severity = Info | Debug | Warning | Error | Critical
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | How a robot log entry was produced.
data RobotLogSource
  = -- | Produced by 'Swarm.Language.Syntax.Say'
    Said
  | -- | Produced by 'Swarm.Language.Syntax.Log'
    Logged
  | -- | Produced as the result of an error.
    RobotError
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | Source of a log entry.
data LogSource
  = -- | Log produced by a robot.  Stores information about which
    --   command was used and the ID of the producing robot.
    RobotLog RobotLogSource Int
  | -- | Log produced by an exception or system.
    SystemLog
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data LogLocation a = Omnipresent | Located a
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | A log entry.
data LogEntry = LogEntry
  { _leTime :: TickNumber
  -- ^ The time at which the entry was created.
  --   Note that this is the first field we sort on.
  , _leSource :: LogSource
  -- ^ Where this log message came from.
  , _leSeverity :: Severity
  -- ^ Severity level of this log message.
  , _leName :: Text
  -- ^ Name of the robot or subsystem that generated this log entry.
  , _leLocation :: LogLocation (Cosmic Location)
  -- ^ Location associated with this log message.
  , _leText :: Text
  -- ^ The text of the log entry.
  }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

makeLenses ''LogEntry
