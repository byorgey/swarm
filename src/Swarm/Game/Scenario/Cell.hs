{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Cell (
  PCell (..),
  Cell,
) where

import Control.Lens hiding (from, (<.>))
import Control.Monad (when)
import Control.Monad.Extra (mapMaybeM)
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Yaml as Y
import Swarm.Game.Entity
import Swarm.Game.Scenario.RobotLookup
import Swarm.Game.Terrain
import Swarm.Util.Yaml

------------------------------------------------------------
-- World cells
------------------------------------------------------------

-- | A single cell in a world map, which contains a terrain value,
--   and optionally an entity and robot.
--   It is parameterized on the Entity type to facilitate less
--   stateful versions of the Entity type in rendering scenario data.
data PCell e = Cell
  { cellTerrain :: TerrainType
  , cellEntity :: Maybe e
  , cellRobots :: [IndexedTRobot]
  }
  deriving (Eq, Show)

-- | A single cell in a world map, which contains a terrain value,
--   and optionally an entity and robot.
type Cell = PCell Entity

-- | Parse a tuple such as @[grass, rock, base]@ into a 'Cell'.  The
--   entity and robot, if present, are immediately looked up and
--   converted into 'Entity' and 'TRobot' values.  If they are not
--   found, a parse error results.
instance FromJSONE (EntityMap, RobotMap) Cell where
  parseJSONE = withArrayE "tuple" $ \v -> do
    let tup = V.toList v
    when (null tup) $ fail "palette entry must nonzero length (terrain, optional entity and then robots if any)"

    terr <- liftE $ parseJSON (head tup)

    ent <- case tup ^? ix 1 of
      Nothing -> return Nothing
      Just e -> do
        meName <- liftE $ parseJSON @(Maybe Text) e
        traverse (localE fst . getEntity) meName

    let name2rob r = do
          mrName <- liftE $ parseJSON @(Maybe Text) r
          traverse (localE snd . getRobot) mrName

    robs <- mapMaybeM name2rob (drop 2 tup)

    return $ Cell terr ent robs
