module Swarm.TUI.Editor.Util where

import Control.Applicative ((<|>))
import Control.Lens hiding (Const, from)
import Control.Monad (guard)
import Data.Int (Int32)
import Data.Map qualified as M
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Vector qualified as V
import Swarm.Game.Entity
import Swarm.Game.Scenario.Topography.Area qualified as EA
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.EntityFacade
import Swarm.Game.Scenario.Topography.WorldDescription
import Swarm.Game.Terrain (TerrainType)
import Swarm.Game.World qualified as W
import Swarm.TUI.Editor.Model
import Swarm.TUI.Model

getEntitiesForList :: EntityMap -> V.Vector EntityFacade
getEntitiesForList em =
  V.fromList $ map mkFacade entities
 where
  entities = M.elems $ entitiesByName em

getEditingBounds :: WorldDescription -> (Bool, W.BoundsRectangle)
getEditingBounds myWorld =
  (EA.isEmpty a, newBounds)
 where
  newBounds = (W.locToCoords upperLeftLoc, W.locToCoords lowerRightLoc)
  upperLeftLoc = ul myWorld
  a = EA.getAreaDimensions $ area myWorld
  lowerRightLoc = EA.upperLeftToBottomRight a upperLeftLoc

getContentAt ::
  WorldEditor Name ->
  W.World Int Entity ->
  W.Coords ->
  (TerrainType, Maybe EntityPaint)
getContentAt editor w coords =
  (terrainWithOverride, entityWithOverride)
 where
  terrainWithOverride = Maybe.fromMaybe underlyingCellTerrain $ do
    (terrainOverride, _) <- maybePaintedCell
    return terrainOverride

  maybeEntityOverride = do
    (_, e) <- maybePaintedCell
    Facade <$> e

  maybePaintedCell = do
    guard $ editor ^. isWorldEditorEnabled
    Map.lookup coords pm

  pm = editor ^. paintedTerrain

  entityWithOverride = (Ref <$> underlyingCellEntity) <|> maybeEntityOverride
  underlyingCellEntity = W.lookupEntity coords w
  underlyingCellTerrain = toEnum $ W.lookupTerrain coords w

getTerrainAt ::
  WorldEditor Name ->
  W.World Int Entity ->
  W.Coords ->
  TerrainType
getTerrainAt editor w coords = fst $ getContentAt editor w coords

isOutsideTopLeftCorner ::
  -- | top left corner coords
  W.Coords ->
  -- | current coords
  W.Coords ->
  Bool
isOutsideTopLeftCorner (W.Coords (yTop, xLeft)) (W.Coords (y, x)) =
  x < xLeft || y < yTop

isOutsideBottomRightCorner ::
  -- | bottom right corner coords
  W.Coords ->
  -- | current coords
  W.Coords ->
  Bool
isOutsideBottomRightCorner (W.Coords (yBottom, xRight)) (W.Coords (y, x)) =
  x > xRight || y > yBottom

isOutsideRegion ::
  -- | full bounds
  W.BoundsRectangle ->
  -- | current coords
  W.Coords ->
  Bool
isOutsideRegion (tl, br) coord =
  isOutsideTopLeftCorner tl coord || isOutsideBottomRightCorner br coord

getEditedMapRectangle ::
  WorldEditor Name ->
  Maybe W.BoundsRectangle ->
  W.World Int Entity ->
  [[CellPaintDisplay]]
getEditedMapRectangle _ Nothing _ = []
getEditedMapRectangle worldEditor (Just coords) w =
  map renderRow [yTop .. yBottom]
 where
  (W.Coords (yTop, xLeft), W.Coords (yBottom, xRight)) = coords

  getContent = getContentAt worldEditor w

  drawCell :: Int32 -> Int32 -> CellPaintDisplay
  drawCell rowIndex colIndex =
    Cell
      terrain
      (toFacade <$> maybeEntity)
      []
   where
    (terrain, maybeEntity) = getContent $ W.Coords (rowIndex, colIndex)

  renderRow rowIndex = map (drawCell rowIndex) [xLeft .. xRight]
