-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Swarm.Game.WorldGen
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Procedural world generation via coherent noise.
module Swarm.Game.WorldGen where

import Data.Bool
import Data.Enumeration
import Data.Hash.Murmur
import Data.Int (Int64)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric.Noise.Perlin
import Numeric.Noise.Ridged
import Witch

import Data.Array.IArray
import Swarm.Game.Terrain
import Swarm.Game.World

-- | A simple test world I used for a while during early development.
testWorld1 :: WorldFun TerrainType Text
testWorld1 (Coords (-5, 3)) = (StoneT, Just "flerb")
testWorld1 (Coords (2, -1)) = (GrassT, Just "elephant")
testWorld1 (Coords (i, j))
  | noiseValue pn1 (fromIntegral i, fromIntegral j, 0) > 0 = (DirtT, Just "tree")
  | noiseValue pn2 (fromIntegral i, fromIntegral j, 0) > 0 = (StoneT, Just "rock")
  | otherwise = (GrassT, Nothing)
 where
  pn1, pn2 :: Perlin
  pn1 = perlin 0 5 0.05 0.5
  pn2 = perlin 0 5 0.05 0.75

data Size = Small | Big deriving (Eq, Ord, Show, Read)
data Hardness = Soft | Hard deriving (Eq, Ord, Show, Read)
data Origin = Natural | Artificial deriving (Eq, Ord, Show, Read)
type Seed = Int

-- | A more featureful test world.
testWorld2 :: Seed -> WorldFun TerrainType Text
testWorld2 baseSeed (Coords ix@(r, c)) =
  genBiome
    (bool Small Big (sample ix pn0 > 0))
    (bool Soft Hard (sample ix pn1 > 0))
    (bool Natural Artificial (sample ix pn2 > 0))
 where
  h = murmur3 0 (into (show ix))

  genBiome Big Hard Natural
    | sample ix cl0 > 0.5 && sample ix rg0 > 0.999 = (StoneT, Just "copper vein")
    | sample ix cl0 > 0.5 && sample ix rg1 > 0.99 = (StoneT, Just "iron vein")
    | sample ix cl0 > 0.5 = (StoneT, Just "mountain")
    | h `mod` 30 == 0 = (StoneT, Just "boulder")
    | sample ix cl0 > 0 = (DirtT, Just "tree")
    | otherwise = (GrassT, Nothing)
  genBiome Small Hard Natural
    | h `mod` 10 == 0 = (StoneT, Just "rock")
    | otherwise = (StoneT, Nothing)
  genBiome Big Soft Natural
    | abs (sample ix pn1) < 0.1 = (DirtT, Just "sand")
    | even (r + c) = (DirtT, Just "wavy water")
    | otherwise = (DirtT, Just "water")
  genBiome Small Soft Natural
    | h `mod` 10 == 0 = (GrassT, Just "flower")
    | otherwise = (GrassT, Nothing)
  genBiome Small Soft Artificial
    | h `mod` 10 == 0 = (GrassT, Just (T.concat ["bit (", from (show ((r + c) `mod` 2)), ")"]))
    | otherwise = (GrassT, Nothing)
  genBiome Big Soft Artificial
    | h `mod` 5000 == 0 = (DirtT, Just "linux")
    | sample ix cl0 > 0.5 = (GrassT, Nothing)
    | otherwise = (DirtT, Nothing)
  genBiome Small Hard Artificial
    | h `mod` 120 == 1 = (StoneT, Just "lambda")
    | otherwise = (StoneT, Nothing)
  genBiome Big Hard Artificial
    | sample ix cl0 > 0.85 = (StoneT, Just "copper ore")
    | otherwise = (StoneT, Nothing)

  sample (i, j) noise = noiseValue noise (fromIntegral i / 2, fromIntegral j / 2, 0)

  pn :: Int -> Perlin
  pn seed = perlin (seed + baseSeed) 6 0.05 0.6

  pn0 = pn 0
  pn1 = pn 1
  pn2 = pn 2

  rg :: Int -> Ridged
  rg seed = ridged seed 6 0.05 1 2

  rg0 = rg 42
  rg1 = rg 66

  clumps :: Int -> Perlin
  clumps seed = perlin (seed + baseSeed) 4 0.08 0.5

  cl0 = clumps 0

-- | Create a world function from a finite array of specified cells
--   plus a seed to randomly generate the rest.
testWorld2FromArray :: Array (Int64, Int64) (TerrainType, Maybe Text) -> Seed -> WorldFun TerrainType Text
testWorld2FromArray arr seed co@(Coords (r, c))
  | inRange bnds (r, c) = arr ! (r, c)
  | otherwise = tw2 co
 where
  tw2 = testWorld2 seed
  bnds = bounds arr

-- | Offset the world so the base starts on empty spot next to tree and grass.
findGoodOrigin :: WorldFun t Text -> WorldFun t Text
findGoodOrigin f = \(Coords (r, c)) -> f (Coords (r + rOffset, c + cOffset))
 where
  int' :: Enumeration Int64
  int' = fromIntegral <$> int
  (rOffset, cOffset) = fromMaybe (error "the impossible happened, no offsets were found") offsets
  offsets = find isGoodPlace (enumerate (int' >< int'))
  hasEntity mayE = (== mayE) . snd . f . Coords
  isGoodPlace cs =
    hasEntity Nothing cs
      && any (hasEntity (Just "tree")) (neighbors cs)
      && all (\c -> hasEntity (Just "tree") c || hasEntity Nothing c) (neighbors cs)
  neighbors (x, y) = (,) <$> [x, x - 1, x + 1] <*> [y, y - 1, y + 1]
