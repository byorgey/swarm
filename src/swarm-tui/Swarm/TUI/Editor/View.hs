-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Editor.View where

import Brick hiding (Direction)
import Brick.Focus
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.List qualified as BL
import Control.Lens hiding (Const, from)
import Data.List qualified as L
import Data.Map qualified as M
import Swarm.Game.Entity.Cosmetic
import Swarm.Game.Land
import Swarm.Game.Scenario
import Swarm.Game.Scenario.Topography.Area qualified as EA
import Swarm.Game.Scenario.Topography.EntityFacade
import Swarm.Game.Terrain (TerrainMap, TerrainType)
import Swarm.Game.Universe
import Swarm.Game.World.Coords
import Swarm.TUI.Border
import Swarm.TUI.Editor.Model
import Swarm.TUI.Model
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.UI.Gameplay
import Swarm.TUI.Panel
import Swarm.TUI.View.Attribute.Attr
import Swarm.TUI.View.CellDisplay (renderDisplay)
import Swarm.TUI.View.Util qualified as VU
import Swarm.Util (applyWhen)

extractTerrainMap :: UIGameplay -> TerrainMap
extractTerrainMap uig =
  maybe mempty (view (scenarioLandscape . scenarioTerrainAndEntities . terrainMap) . fst) $
    uig ^. scenarioRef

drawWorldEditor :: FocusRing Name -> UIGameplay -> Widget Name
drawWorldEditor toplevelFocusRing uig =
  if worldEditor ^. worldOverdraw . isWorldEditorEnabled
    then
      panel
        highlightAttr
        toplevelFocusRing
        (FocusablePanel WorldEditorPanel)
        plainBorder
        innerWidget
    else emptyWidget
 where
  privateFocusRing = worldEditor ^. editorFocusRing
  maybeCurrentFocus = focusGetCurrent privateFocusRing

  controlsBox =
    padBottom Max $
      vBox
        [ brushWidget
        , entityWidget
        , clearEntityButtonWidget
        , areaWidget
        , outputWidget
        , str " "
        , saveButtonWidget
        ]

  innerWidget =
    padLeftRight 1 $
      hLimit 30 $
        controlsBox <=> statusBox

  worldEditor = uig ^. uiWorldEditor
  maybeAreaBounds = worldEditor ^. editingBounds . boundsRect

  -- TODO (#1150): Use withFocusRing?
  mkFormControl n w =
    clickable n $ transformation w
   where
    transformation =
      applyWhen (Just n == maybeCurrentFocus) $
        withAttr BL.listSelectedFocusedAttr

  swatchContent list drawFunc =
    maybe emptyWidget drawFunc selectedThing
   where
    selectedThing = snd <$> BL.listSelectedElement list

  tm = extractTerrainMap uig
  aMap = maybe mempty (view (scenarioLandscape . scenarioCosmetics) . fst) $ uig ^. scenarioRef

  brushWidget =
    mkFormControl (WorldEditorPanelControl BrushSelector) $
      padRight (Pad 1) (str "Brush:")
        <+> swatchContent (worldEditor ^. terrainList) (VU.drawLabeledTerrainSwatch aMap tm)

  entityWidget =
    mkFormControl (WorldEditorPanelControl EntitySelector) $
      padRight (Pad 1) (str "Entity:")
        <+> swatchContent (worldEditor ^. entityPaintList) (drawLabeledEntitySwatch aMap)

  clearEntityButtonWidget =
    if null $ worldEditor ^. entityPaintList . BL.listSelectedL
      then emptyWidget
      else
        mkFormControl (WorldEditorPanelControl ClearEntityButton)
          . hLimit 20
          . hCenter
          $ str "None"

  areaContent = case worldEditor ^. editingBounds . boundsSelectionStep of
    UpperLeftPending -> str "Click top-left"
    LowerRightPending _wcoords -> str "Click bottom-right"
    SelectionComplete -> maybe emptyWidget (renderBounds . view planar) maybeAreaBounds

  areaWidget =
    mkFormControl (WorldEditorPanelControl AreaSelector) $
      vBox
        [ str "Area:"
        , areaContent
        ]

  renderBounds (upperLeftCoord, lowerRightCoord) =
    str $
      unwords $
        L.intersperse
          "@"
          [ EA.renderRectDimensions rectArea
          , locationToString upperLeftLoc
          ]
   where
    upperLeftLoc = coordsToLoc upperLeftCoord
    lowerRightLoc = coordsToLoc lowerRightCoord
    rectArea = EA.cornersToArea upperLeftLoc lowerRightLoc

  outputWidget =
    mkFormControl (WorldEditorPanelControl OutputPathSelector) $
      padRight (Pad 1) (str "Output:") <+> outputWidgetContent

  outputWidgetContent = str $ worldEditor ^. outputFilePath

  saveButtonWidget =
    mkFormControl (WorldEditorPanelControl MapSaveButton)
      . hLimit 20
      . hCenter
      $ str "Save"

  statusBox = maybe emptyWidget str $ worldEditor ^. lastWorldEditorMessage

drawLabeledEntitySwatch ::
  M.Map WorldAttr PreservableColor ->
  EntityFacade ->
  Widget Name
drawLabeledEntitySwatch aMap (EntityFacade eName eDisplay) =
  tile <+> txt eName
 where
  tile = padRight (Pad 1) $ renderDisplay aMap eDisplay

drawTerrainSelector :: UIGameplay -> Widget Name
drawTerrainSelector uig =
  padAll 1
    . hCenter
    . vLimit 8
    . BL.renderListWithIndex (listDrawTerrainElement aMap $ extractTerrainMap uig) True
    $ uig ^. uiWorldEditor . terrainList
 where
  aMap =
    maybe mempty (view (scenarioLandscape . scenarioCosmetics) . fst) $
      uig ^. scenarioRef

listDrawTerrainElement ::
  M.Map WorldAttr PreservableColor ->
  TerrainMap ->
  Int ->
  Bool ->
  TerrainType ->
  Widget Name
listDrawTerrainElement aMap tm pos _isSelected a =
  clickable (TerrainListItem pos) $ VU.drawLabeledTerrainSwatch aMap tm a

drawEntityPaintSelector :: UIGameplay -> Widget Name
drawEntityPaintSelector uig =
  padAll 1
    . hCenter
    . vLimit 10
    . BL.renderListWithIndex (listDrawEntityPaintElement aMap) True
    $ uig ^. uiWorldEditor . entityPaintList
 where
  aMap =
    maybe mempty (view (scenarioLandscape . scenarioCosmetics) . fst) $
      uig ^. scenarioRef

listDrawEntityPaintElement ::
  M.Map WorldAttr PreservableColor ->
  Int ->
  Bool ->
  EntityFacade ->
  Widget Name
listDrawEntityPaintElement aMap pos _isSelected =
  clickable (EntityPaintListItem pos) . drawLabeledEntitySwatch aMap
