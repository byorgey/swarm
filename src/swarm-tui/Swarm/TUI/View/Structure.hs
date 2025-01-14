{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Display logic for Structures.
module Swarm.TUI.View.Structure (
  renderStructuresDisplay,
  makeListWidget,
) where

import Brick hiding (Direction, Location, getName)
import Brick.Focus
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.List qualified as BL
import Control.Lens hiding (Const, from)
import Data.Map.NonEmpty qualified as NEM
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as V
import Swarm.Game.Entity (Entity, entityDisplay)
import Swarm.Game.Entity.Cosmetic
import Swarm.Game.Scenario
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.Grid
import Swarm.Game.Scenario.Topography.Structure.Named qualified as Structure
import Swarm.Game.Scenario.Topography.Structure.Recognition (foundStructures)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Registry (foundByName)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Static
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.State
import Swarm.Game.State.Substate (structureRecognition)
import Swarm.Language.Syntax.Direction (directionJsonModifier)
import Swarm.TUI.Model.Dialog.Structure
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.UI.Gameplay
import Swarm.TUI.View.Attribute.Attr
import Swarm.TUI.View.CellDisplay
import Swarm.TUI.View.Shared (tabControlFooter)
import Swarm.TUI.View.Util
import Swarm.Util (commaList)

-- | Render a two-pane widget with structure selection on the left
-- and single-structure details on the right.
structureWidget ::
  M.Map WorldAttr PreservableColor ->
  GameState ->
  StructureInfo b Entity ->
  Widget n
structureWidget aMap gs s =
  vBox
    [ hBox
        [ headerItem "Name" $ Structure.getStructureName theName
        , padLeft (Pad 2)
            . headerItem "Size"
            . T.pack
            . renderRectDimensions
            . getNEGridDimensions
            $ entityProcessedGrid s
        , occurrenceCountSuffix
        ]
    , reorientabilityWidget
    , maybeDescriptionWidget
    , padTop (Pad 1) $
        hBox
          [ structureIllustration
          , padLeft (Pad 4) ingredientsBox
          ]
    ]
 where
  headerItem h content =
    hBox
      [ padRight (Pad 1) $ txt $ h <> ":"
      , withAttr boldAttr $ txt content
      ]

  annotatedStructureGrid = annotatedGrid s
  theNamedGrid = originalItem $ grid annotatedStructureGrid
  supportedOrientations = Set.toList . Structure.recognize $ theNamedGrid

  renderSymmetry = \case
    NoSymmetry -> "no"
    TwoFold -> "2-fold"
    FourFold -> "4-fold"

  reorientabilityWidget =
    txt $
      T.unwords
        [ "Orientable:"
        , commaList $ map (T.pack . directionJsonModifier . show) supportedOrientations
        , "with"
        , renderSymmetry $ symmetry annotatedStructureGrid
        , "rotational symmetry."
        ]

  maybeDescriptionWidget =
    maybe emptyWidget (padTop (Pad 1) . withAttr italicAttr . txtWrap) $
      Structure.description theNamedGrid

  registry = gs ^. discovery . structureRecognition . foundStructures
  occurrenceCountSuffix = case M.lookup theName $ foundByName registry of
    Nothing -> emptyWidget
    Just inner -> padLeft (Pad 2) . headerItem "Count" . T.pack . show $ NEM.size inner

  structureIllustration = vBox $ map (hBox . map renderOneCell) cells

  ingredientsBox =
    vBox
      [ padBottom (Pad 1) $ withAttr boldAttr $ txt "Materials:"
      , ingredientLines
      ]
  ingredientLines = vBox . map showCount . M.toList $ entityCounts s

  showCount (e, c) =
    hBox
      [ drawLabelledEntityName aMap e
      , txt $
          T.unwords
            [ ":"
            , T.pack $ show c
            ]
      ]

  theName = Structure.name theNamedGrid
  cells = getRows $ Grid $ entityProcessedGrid s

  renderOneCell = maybe (txt " ") (renderDisplay aMap . view entityDisplay)

makeListWidget :: [StructureInfo b a] -> BL.List Name (StructureInfo b a)
makeListWidget structureDefinitions =
  BL.listMoveTo 0 $ BL.list (StructureWidgets StructuresList) (V.fromList structureDefinitions) 1

renderStructuresDisplay ::
  GameState ->
  UIGameplay ->
  Widget Name
renderStructuresDisplay gs uig =
  vBox
    [ hBox
        [ leftSide
        , padLeft (Pad 2) structureElaboration
        ]
    , tabControlFooter
    ]
 where
  aMap = maybe mempty (view (scenarioLandscape . scenarioCosmetics) . fst) $ uig ^. scenarioRef

  structureDisplay = uig ^. uiDialogs . uiStructure
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
      . maybe emptyWidget (padAll 1 . padRight (Pad 1) . highlightIfFocused . structureWidget aMap gs . snd)
      $ BL.listSelectedElement lw

drawSidebarListItem ::
  Bool ->
  StructureInfo b a ->
  Widget Name
drawSidebarListItem _isSelected (StructureInfo annotated _ _) =
  txt . Structure.getStructureName . Structure.name $ originalItem $ grid annotated
