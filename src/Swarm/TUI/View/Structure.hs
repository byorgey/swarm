{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Display logic for Objectives.
module Swarm.TUI.View.Structure (
  renderStructuresDisplay,
  makeListWidget,
) where

import Brick hiding (Direction, Location)
import Brick.Focus
import Brick.Widgets.Center
import Brick.Widgets.List qualified as BL
import Control.Lens hiding (Const, from)
import Data.Map.NonEmpty qualified as NEM
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Swarm.Game.Entity (entityDisplay, entityName)
import Swarm.Game.Scenario.Topography.Placement
import Swarm.Game.Scenario.Topography.Structure qualified as Structure
import Swarm.Game.Scenario.Topography.Structure.Recognition.Precompute (getEntityGrid)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Registry (foundByName)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type.Toplevel (foundStructures)
import Swarm.Game.State
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.Structure
import Swarm.TUI.View.Attribute.Attr
import Swarm.TUI.View.CellDisplay
import Swarm.TUI.View.Util
import Swarm.Util (parens)

structureWidget :: GameState -> StructureInfo -> Widget n
structureWidget gs s =
  hLimit 30 $
    vBox
      [ hBox
          [ padRight (Pad 1) $ txt "Name:"
          , withAttr boldAttr $ txt theName
          , txt occurrenceCountSuffix
          ]
      , maybeDescriptionWidget
      , padTop (Pad 1) $
          hBox
            [ structureIllustration
            , padLeft (Pad 4) ingredientsBox
            ]
      ]
 where
  maybeDescriptionWidget = maybe emptyWidget txtWrap $ Structure.description . originalDefinition . withGrid $ s

  registry = gs ^. discovery . structureRecognition . foundStructures
  occurrenceCountSuffix = case M.lookup sName $ foundByName registry of
    Nothing -> ""
    Just inner -> " " <> parens (T.unwords [T.pack $ show $ NEM.size inner, "found"])

  structureIllustration = vBox $ map (hBox . map renderOneCell) cells
  d = originalDefinition $ withGrid s

  ingredientsBox =
    vBox
      [ padBottom (Pad 1) $ withAttr boldAttr $ txt "Ingredients:"
      , ingredientLines
      ]
  ingredientLines = vBox . map showCount . M.toList $ entityCounts s

  showCount (e, c) =
    txt $
      T.unwords
        [ view entityName e <> ":"
        , T.pack $ show c
        ]

  sName = Structure.name d
  StructureName theName = sName
  cells = getEntityGrid d
  renderOneCell = maybe (txt " ") (renderDisplay . view entityDisplay)

makeListWidget :: [StructureInfo] -> BL.List Name StructureInfo
makeListWidget structureDefs =
  BL.listMoveTo 0 $ BL.list (StructureWidgets StructuresList) (V.fromList structureDefs) 1

renderStructuresDisplay :: GameState -> StructureDisplay -> Widget Name
renderStructuresDisplay gs structureDisplay =
  vBox
    [ hBox
        [ leftSide
        , padLeft (Pad 2) structureElaboration
        ]
    , footer
    ]
 where
  footer = hCenter $ withAttr italicAttr $ txt "NOTE: [Tab] toggles focus between panes"
  lw = _structurePanelListWidget structureDisplay
  fr = _structurePanelFocus structureDisplay
  leftSide =
    hLimitPercent 25 $
      padAll 1 $
        vBox
          [ hCenter $ withAttr boldAttr $ txt "Candidates"
          , padAll 1 $
              vLimit 10 $
                withFocusRing fr (BL.renderList drawSidebarListItem) lw
          ]

  -- Adds very subtle coloring to indicate focus switch
  highlightIfFocused = case focusGetCurrent fr of
    Just (StructureWidgets StructureSummary) -> withAttr lightCyanAttr
    _ -> id

  -- Note: An extra "padRight" is inserted to account for the vertical scrollbar,
  -- whether or not it appears.
  structureElaboration =
    clickable (StructureWidgets StructureSummary)
      . maybeScroll ModalViewport
      . maybe emptyWidget (padAll 1 . padRight (Pad 1) . highlightIfFocused . structureWidget gs . snd)
      $ BL.listSelectedElement lw

drawSidebarListItem ::
  Bool ->
  StructureInfo ->
  Widget Name
drawSidebarListItem _isSelected (StructureInfo swg _) =
  txt . getStructureName . Structure.name $ originalDefinition swg
