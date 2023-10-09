-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Modifying the world
module Swarm.Game.World.Modify where

import Control.Lens
import Data.Function (on)
import Swarm.Game.Entity (Entity, entityHash)

-- | Compare to 'WorldUpdate' in "Swarm.Game.World"
data CellUpdate e
  = NoChange (Maybe e)
  | Modified (CellModification e)

getModification :: CellUpdate e -> Maybe (CellModification e)
getModification (NoChange _) = Nothing
getModification (Modified x) = Just x

data CellModification e
  = Swap
      -- | before
      e
      -- | after
      e
  | Remove e
  | Add e

classifyModification ::
  -- | before
  Maybe Entity ->
  -- | after
  Maybe Entity ->
  CellUpdate Entity
classifyModification Nothing Nothing = NoChange Nothing
classifyModification Nothing (Just x) = Modified $ Add x
classifyModification (Just x) Nothing = Modified $ Remove x
classifyModification (Just x) (Just y) =
  if ((/=) `on` view entityHash) x y
    then NoChange $ Just x
    else Modified $ Swap x y