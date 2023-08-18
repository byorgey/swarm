{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Editor.Model where

import Brick.Focus
import Brick.Widgets.List qualified as BL
import Control.Lens hiding (from, (.=), (<.>))
import Data.Map qualified as M
import Data.Vector qualified as V
import Swarm.Game.Display (Display)
import Swarm.Game.Entity qualified as E
import Swarm.Game.Scenario.Topography.EntityFacade
import Swarm.Game.Scenario.Topography.WorldPalette
import Swarm.Game.Terrain (TerrainType)
import Swarm.Game.Universe
import Swarm.Game.World qualified as W
import Swarm.TUI.Model.Name
import Swarm.Util
import System.Clock

data BoundsSelectionStep
  = UpperLeftPending
  | -- | Stores the *world coords* of the upper-left click
    LowerRightPending (Cosmic W.Coords)
  | SelectionComplete

data EntityPaint
  = Facade EntityFacade
  | Ref E.Entity
  deriving (Eq)

getDisplay :: EntityPaint -> Display
getDisplay (Facade (EntityFacade _ d)) = d
getDisplay (Ref e) = e ^. E.entityDisplay

toFacade :: EntityPaint -> EntityFacade
toFacade = \case
  Facade f -> f
  Ref e -> mkFacade e

getEntityName :: EntityFacade -> E.EntityName
getEntityName (EntityFacade name _) = name

data MapEditingBounds = MapEditingBounds
  { _boundsRect :: Maybe (Cosmic W.BoundsRectangle)
  -- ^ Upper-left and lower-right coordinates
  -- of the map to be saved.
  , _boundsPersistDisplayUntil :: TimeSpec
  , _boundsSelectionStep :: BoundsSelectionStep
  }

makeLenses ''MapEditingBounds

data WorldOverdraw = WorldOverdraw {
    _isWorldEditorEnabled :: Bool
    -- ^ This field has deferred initialization; it gets populated when a game
    -- is initialized.
  , _paintedTerrain :: M.Map W.Coords (TerrainWith EntityFacade)
  }

makeLenses ''WorldOverdraw

data WorldEditor n = WorldEditor
  { _worldOverdraw :: WorldOverdraw
  , _terrainList :: BL.List n TerrainType
  , _entityPaintList :: BL.List n EntityFacade
  , _editingBounds :: MapEditingBounds
  , _editorFocusRing :: FocusRing n
  , _outputFilePath :: FilePath
  , _lastWorldEditorMessage :: Maybe String
  }

makeLenses ''WorldEditor

initialWorldEditor :: TimeSpec -> WorldEditor Name
initialWorldEditor ts =
  WorldEditor
    (WorldOverdraw False mempty)
    (BL.list TerrainList (V.fromList listEnums) 1)
    (BL.list EntityPaintList (V.fromList []) 1)
    bounds
    (focusRing $ map WorldEditorPanelControl listEnums)
    "mymap.yaml"
    Nothing
 where
  bounds =
    MapEditingBounds
      -- Note that these are in "world coordinates",
      -- not in player-facing "Location" coordinates
      (Just $ Cosmic DefaultRootSubworld (W.Coords (-10, -20), W.Coords (10, 20)))
      (ts - 1)
      SelectionComplete
