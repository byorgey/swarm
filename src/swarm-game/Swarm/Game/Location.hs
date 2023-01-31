{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Orphan JSON instances for Location and Heading

-- |
-- Module      :  Swarm.Game.Location
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Locations and headings.
module Swarm.Game.Location (
  Location,
  pattern Location,
  Heading,
  origin,

  -- ** utility functions
  manhattan,
  getElemsInArea,
) where

import Data.Aeson (FromJSONKey, ToJSONKey)
import Data.Function ((&))
import Data.Int (Int32)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Yaml (FromJSON (parseJSON), ToJSON (toJSON))
import Linear (V2 (..))
import Linear.Affine (Point (..), origin)

-- | A Location is a pair of (x,y) coordinates, both up to 32 bits.
--   The positive x-axis points east and the positive y-axis points
--   north.  These are the coordinates that are shown to players.
--
--   See also the 'Coords' type defined in "Swarm.Game.World", which
--   use a (row, column) format instead, which is more convenient for
--   internal use.  The "Swarm.Game.World" module also defines
--   conversions between 'Location' and 'Coords'.
type Location = Point V2 Int32

-- | A convenient way to pattern-match on 'Location' values.
pattern Location :: Int32 -> Int32 -> Location
pattern Location x y = P (V2 x y)

{-# COMPLETE Location #-}

-- | A @Heading@ is a 2D vector, with 32-bit coordinates.
--
--   'Location' and 'Heading' are both represented using types from
--   the @linear@ package, so they can be manipulated using a large
--   number of operators from that package.  For example:
--
--   * Two headings can be added with '^+^'.
--   * The difference between two 'Location's is a 'Heading' (via '.-.').
--   * A 'Location' plus a 'Heading' is another 'Location' (via '.^+').
type Heading = V2 Int32

deriving instance ToJSON (V2 Int32)
deriving instance FromJSON (V2 Int32)

deriving instance FromJSONKey (V2 Int32)
deriving instance ToJSONKey (V2 Int32)

instance FromJSON Location where
  parseJSON = fmap P . parseJSON

instance ToJSON Location where
  toJSON (P v) = toJSON v

-- | Manhattan distance between world locations.
manhattan :: Location -> Location -> Int32
manhattan (Location x1 y1) (Location x2 y2) = abs (x1 - x2) + abs (y1 - y2)

-- | Get elements that are in manhattan distance from location.
--
-- >>> v2s i = [(p, manhattan origin p) | x <- [-i..i], y <- [-i..i], let p = Location x y]
-- >>> v2s 0
-- [(P (V2 0 0),0)]
-- >>> map (\i -> length (getElemsInArea origin i (M.fromList $ v2s i))) [0..8]
-- [1,5,13,25,41,61,85,113,145]
--
-- The last test is the sequence "Centered square numbers":
-- https://oeis.org/A001844
getElemsInArea :: Location -> Int32 -> Map Location e -> [e]
getElemsInArea o@(Location x y) d m = M.elems sm'
 where
  -- to be more efficient we basically split on first coordinate
  -- (which is logarithmic) and then we have to linearly filter
  -- the second coordinate to get a square - this is how it looks:
  --         ▲▲▲▲
  --         ││││    the arrows mark points that are greater then A
  --         ││s│                                 and lesser then B
  --         │sssB (2,1)
  --         ssoss   <-- o=(x=0,y=0) with d=2
  -- (-2,-1) Asss│
  --          │s││   the point o and all s are in manhattan
  --          ││││                  distance 2 from point o
  --          ▼▼▼▼
  sm =
    m
      & M.split (Location (x - d) (y - 1)) -- A
      & snd -- A<
      & M.split (Location (x + d) (y + 1)) -- B
      & fst -- B>
  sm' = M.filterWithKey (const . (<= d) . manhattan o) sm
