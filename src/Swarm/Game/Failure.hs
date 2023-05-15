-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A data type to represent system failures.
--
-- These failures are often not fatal and serve
-- to create common infrastructure for logging.
module Swarm.Game.Failure where

import Data.Text (Text)
import Data.Yaml (ParseException)

data SystemFailure
  = AssetNotLoaded Asset FilePath LoadingFailure

data AssetData = AppAsset | NameGeneration | Entities | Recipes | Scenarios | Script
  deriving (Eq, Show)

data Asset = Achievement | Data AssetData | History | Save
  deriving (Show)

data Entry = Directory | File
  deriving (Eq, Show)

data LoadingFailure
  = DoesNotExist Entry
  | EntryNot Entry
  | CanNotParse ParseException
  | CustomMessage Text
