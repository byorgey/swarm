{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Terrain types and properties.
module Swarm.Game.Terrain (
  -- * Terrain
  TerrainType (..),
  terrainMap,
  getTerrainDefaultPaletteChar,
  getTerrainWord,
) where

import Data.Aeson (FromJSON (..), withText)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text qualified as T
import Swarm.Game.Display
import Swarm.Util (failT)
import Text.Read (readMaybe)
import Witch (into)

-- | The different possible types of terrain. Unlike entities and
--   robots, these are hard-coded into the game.
data TerrainType
  = StoneT
  | DirtT
  | GrassT
  | IceT
  | BlankT
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

instance FromJSON TerrainType where
  parseJSON = withText "text" $ \t ->
    case readMaybe (into @String (T.toTitle t) ++ "T") of
      Just ter -> return ter
      Nothing -> failT ["Unknown terrain type:", t]

getTerrainDefaultPaletteChar :: TerrainType -> Char
getTerrainDefaultPaletteChar = head . show

getTerrainWord :: TerrainType -> T.Text
getTerrainWord = T.toLower . T.pack . init . show

-- | A map containing a 'Display' record for each different 'TerrainType'.
terrainMap :: Map TerrainType Display
terrainMap =
  M.fromList
    [ (StoneT, defaultTerrainDisplay '▒' (ATerrain "stone"))
    , (DirtT, defaultTerrainDisplay '▒' (ATerrain "dirt"))
    , (GrassT, defaultTerrainDisplay '▒' (ATerrain "grass"))
    , (IceT, defaultTerrainDisplay ' ' (ATerrain "ice"))
    , (BlankT, defaultTerrainDisplay ' ' ADefault)
    ]
