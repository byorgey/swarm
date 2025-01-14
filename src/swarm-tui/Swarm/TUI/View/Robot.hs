{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A UI-centric model for presentation of Robot details.
module Swarm.TUI.View.Robot (
  emptyRobotDisplay,
  RobotRenderingContext (..),
  mkRobotDisplay,
  getList,
  updateList,
  -- renderRobotsList,
  renderDutyCycle,
  drawRobotsModal,
) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.List qualified as BL
import Brick.Widgets.TabularList.Mixed
import Control.Lens hiding (from, (<.>))
import Control.Lens as Lens hiding (Const, from)
import Data.IntMap qualified as IM
import Data.List (mapAccumL)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as S
import Data.Set (Set)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Linear (V2 (..), distance)
import Numeric (showFFloat)
import Swarm.Game.CESK (CESK (..))
import Swarm.Game.Entity as E
import Swarm.Game.Location
import Swarm.Game.Robot
import Swarm.Game.Robot.Activity
import Swarm.Game.Robot.Concrete
import Swarm.Game.Scenario
import Swarm.Game.State
import Swarm.Game.State.Robot
import Swarm.Game.State.Substate
import Swarm.Game.Tick (addTicks)
import Swarm.Game.Universe
import Swarm.Game.World.Coords
import Swarm.TUI.Model.DebugOption
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.UI.Gameplay
import Swarm.TUI.View.Attribute.Attr
import Swarm.TUI.View.CellDisplay
import Swarm.TUI.View.Robot.Details
import Swarm.TUI.View.Robot.Type
import Swarm.TUI.View.Shared (tabControlFooter)
import Swarm.Util (applyWhen, maximum0, maximumNE)
import Swarm.Util.UnitInterval
import Swarm.Util.WindowedCounter qualified as WC
import System.Clock (TimeSpec (..))

extractColWidth :: ColWidth -> Int
extractColWidth (ColW x) = x

getMaxWidth :: ColWidth -> ColWidth -> ColWidth
getMaxWidth (ColW w1) (ColW w2) = ColW $ max w1 w2

data RobotRenderingContext = RobotRenderingContext
  { _mygs :: GameState
  , _gameplay :: UIGameplay
  , _timing :: UITiming
  , _uiDbg :: Set DebugOption
  }

makeLenses ''RobotRenderingContext

mkRobotDisplay :: RobotRenderingContext -> MixedTabularList Name RobotWidgetRow Widths
mkRobotDisplay c =
  mixedTabularList
    (RobotsListDialog RobotList)
    (mkLibraryEntries c)
    (LstItmH 1)
    (computeColumnWidths $ c ^. uiDbg)
    assignRowWidthForConstructor

emptyRobotDisplay :: Set DebugOption -> RobotListContent
emptyRobotDisplay uiDebug =
  RobotListContent
    { _robotsListWidget = mixedTabularList (RobotsListDialog RobotList) mempty (LstItmH 1) (computeColumnWidths uiDebug) assignRowWidthForConstructor
    , _robotsListRenderers =
        MixedRenderers
          { cell = drawCell uiDebug
          , rowHdr = Just rowHdr
          , colHdr = Just $ colHdr uiDebug
          , colHdrRowHdr = Just $ ColHdrRowHdr $ \_ _ -> vLimit 1 (fill ' ') <=> hBorder
          }
    , _robotDetailsPaneState =
        RobotDetailsPaneState
          { _logsList = BL.list (RobotsListDialog $ SingleRobotDetails RobotLogPane) mempty 1
          , _cmdHistogramList = BL.list (RobotsListDialog $ SingleRobotDetails RobotCommandHistogramPane) mempty 1
          }
    }

renderRobotsList :: RobotListContent -> Widget Name
renderRobotsList rd =
  vLimit 30 $
    renderMixedTabularList (rd ^. robotsListRenderers) (LstFcs True) (rd ^. robotsListWidget)

columnHdrAttr :: AttrName
columnHdrAttr = attrName "columnHeader"

rowHdrAttr :: AttrName
rowHdrAttr = attrName "rowHeader"

colHdr :: Set DebugOption -> MixedColHdr Name Widths
colHdr uiDebug =
  MixedColHdr
    { draw = \_ (MColC (Ix ci)) -> case hdrs V.!? ci of
        Just ch -> withAttr columnHdrAttr (str ch) <=> hBorder
        Nothing -> emptyWidget
    , widths = \(Widths ws) -> zipWith getMaxWidth ws (map (ColW . length) $ V.toList hdrs)
    , height = ColHdrH 2
    }
 where
  hdrs = colHdrs uiDebug

-- | Enumerates the rows by position (not 'RID').
rowHdr :: RowHdr Name RobotWidgetRow
rowHdr =
  RowHdr
    { draw = \_ (WdthD wd) (RowHdrCtxt (Sel s)) rh ->
        let attrFn = applyWhen (not s) $ withAttr rowHdrAttr
         in attrFn $ padRight (Pad $ if wd > 0 then 0 else 1) $ padLeft Max (str $ show rh)
    , width = \_ rh -> RowHdrW . (+ 2) . maximum0 $ map (length . show) rh
    , toRH = \_ (Ix i) -> i + 1
    }

-- | Note that with the current two constructors, this happens to be analogous
-- to 'Brick.Types.Size'. However, we'd like to reserve the right to add
-- more constructors/growth modes and not be bound by the Brick semantics, so
-- we have our own definition here.
data ColumnExpansion
  = Grow
  | Minimal
  deriving (Eq)

data ColumnAttributes = Col
  { headingString :: String
  , expansionPolicy :: ColumnExpansion
  }

getColumnAttrList :: Set DebugOption -> NonEmpty ColumnAttributes
getColumnAttrList dbgOptions =
  NE.map ($ headingStrings) $ getAccessorList dbgOptions
 where
  headingStrings =
    RobotRow
      { rowID = Col "ID" Minimal
      , rowName = Col "Name" Grow
      , rowAge = Col "Age" Minimal
      , rowPos = Col "Pos" Minimal
      , rowItems = Col "Items" Minimal
      , rowStatus = Col "Status" Minimal
      , rowActns = Col "Actns" Minimal
      , rowCmds = Col "Cmds" Minimal
      , rowCycles = Col "Cycles" Minimal
      , rowActivity = Col "Activity" Grow
      , rowLog = Col "Log" Minimal
      }

colHdrs :: Set DebugOption -> Vector String
colHdrs =
  V.fromList
    . NE.toList
    . NE.map headingString
    . getColumnAttrList

getAccessorList :: Set DebugOption -> NonEmpty (RobotRow a -> a)
getAccessorList dbgOptions =
  applyWhen debugRID (NE.cons rowID) mainListSuffix
 where
  debugRID = dbgOptions ^. Lens.contains ListRobotIDs

  mainListSuffix =
    rowName
      :| [ rowAge
         , rowPos
         , rowItems
         , rowStatus
         , rowActns
         , rowCmds
         , rowCycles
         , rowActivity
         , rowLog
         ]

drawCell :: Set DebugOption -> ListFocused -> MixedCtxt -> RobotWidgetRow -> Widget Name
drawCell uiDebug _ (MxdCtxt _ (MColC (Ix ci))) r =
  maybe emptyWidget (renderPlainCell . wWidget . ($ view row r)) (indexedAccessors V.!? ci)
 where
  indexedAccessors = V.fromList $ NE.toList accessors
  accessors = getAccessorList uiDebug
  renderPlainCell = padRight Max

-- | For a single-constructor datatype like 'RobotWidgetRow',
-- this implementation is trivial.
assignRowWidthForConstructor :: WidthsPerRow RobotWidgetRow Widths
assignRowWidthForConstructor = WsPerR $ \(Widths x) _ -> x

-- |
-- First, computes the minimum width for each column, using
-- both the header string width and the widest visible cell content,
-- then adding 1 to the result of each column for padding.
--
-- Second, to utilize the full available width for the table, distributes
-- the extra space equally among columns marked as 'Grow'.
computeColumnWidths :: Set DebugOption -> WidthsPerRowKind RobotWidgetRow Widths
computeColumnWidths uiDebug = WsPerRK $ \availableWidth allRows ->
  let output = maybe [] (NE.toList . distributeWidths availableWidth) $ NE.nonEmpty allRows
   in Widths {robotRowWidths = output}
 where
  distributeWidths (AvlW availableWidth) allRows =
    NE.zipWith (\(ColW w) extra -> ColW $ w + extra) minWidthsPerColum distributedRemainderSpace
   where
    minWidthsPerColum = mkWidths allRows
    totalRequiredWidth = sum $ NE.map extractColWidth minWidthsPerColum
    spareWidth = availableWidth - totalRequiredWidth

    growPolicies = NE.map expansionPolicy colAttrList
    growableColumnCount = length $ filter (== Grow) $ NE.toList growPolicies
    (spacePerGrowable, remainingSpace) = spareWidth `divMod` growableColumnCount

    distributedRemainderSpace = snd $ mapAccumL addedWidth remainingSpace growPolicies
    addedWidth remainder policy = case policy of
      Grow -> (remainder - 1, spacePerGrowable + extra)
       where
        extra = fromEnum $ remainder > 0
      Minimal -> (remainder, 0)

  colAttrList = getColumnAttrList uiDebug
  colHeaderRowLengths = NE.map (length . headingString) colAttrList

  -- We take the maximum of all cell widths, including the headers, and
  -- add 1 for "padding".
  -- NOTE: We don't necessarily need to pad the last column, but it's
  -- simpler this way and it looks fine.
  mkWidths :: NonEmpty RobotWidgetRow -> NonEmpty ColWidth
  mkWidths =
    NE.map (ColW . (+ 1) . maximumNE)
      . NE.transpose
      . (colHeaderRowLengths `NE.cons`)
      . NE.map getColWidthsForRow
   where
    getColWidthsForRow :: RobotWidgetRow -> NonEmpty Int
    getColWidthsForRow r = NE.map (wWidth . ($ view row r)) $ getAccessorList uiDebug

getList :: MixedTabularList n e w -> BL.GenericList n Seq e
getList (MixedTabularList oldList _ _) = oldList

updateList ::
  (BL.GenericList n1 Seq e -> BL.GenericList n2 Seq e) ->
  MixedTabularList n1 e w ->
  MixedTabularList n2 e w
updateList f (MixedTabularList ls a b) = MixedTabularList (f ls) a b

strWidget :: String -> WidthWidget
strWidget tx = WithWidth (length tx) (str tx)

-- | Render the percentage of ticks that this robot was active.
-- This indicator can take some time to "warm up" and stabilize
-- due to the sliding window.
--
-- == Use of previous tick
-- The 'Swarm.Game.Step.gameTick' function runs all robots, then increments the current tick.
-- So at the time we are rendering a frame, the current tick will always be
-- strictly greater than any ticks stored in the 'WC.WindowedCounter' for any robot;
-- hence 'WC.getOccupancy' will never be @1@ if we use the current tick directly as
-- obtained from the 'ticks' function.
-- So we "rewind" it to the previous tick for the purpose of this display.
renderDutyCycle :: TemporalState -> Robot -> WidthWidget
renderDutyCycle temporalState r =
  withAttr dutyCycleAttr <$> strWidget tx
 where
  tx = showFFloat (Just 1) dutyCyclePercentage "%"

  curTicks = temporalState ^. ticks
  window = r ^. activityCounts . activityWindow

  -- Rewind to previous tick
  latestRobotTick = addTicks (-1) curTicks
  dutyCycleRatio = WC.getOccupancy latestRobotTick window

  dutyCycleAttr = safeIndex dutyCycleRatio meterAttributeNames

  dutyCyclePercentage :: Double
  dutyCyclePercentage = 100 * getValue dutyCycleRatio

mkLibraryEntries :: RobotRenderingContext -> Seq RobotWidgetRow
mkLibraryEntries c =
  mkRobotRow <$> S.fromList robots
 where
  mkRobotRow r =
    RobotRowPayload r $
      RobotRow
        { rowID = strWidget $ show $ r ^. robotID
        , rowName = nameWidget
        , rowAge = strWidget ageStr
        , rowPos = locWidget
        , rowItems = increaseWidth 1 $ strWidget $ show rInvCount
        , rowStatus = statusWidget
        , rowActns = strWidget $ show $ r ^. activityCounts . tangibleCommandCount
        , rowCmds = strWidget $ show . sum . M.elems $ r ^. activityCounts . commandsHistogram
        , rowCycles = strWidget $ show $ r ^. activityCounts . lifetimeStepCount
        , rowActivity = renderDutyCycle (c ^. mygs . temporal) r
        , rowLog = strWidget $ pure rLog
        }
   where
    aMap = maybe mempty (view (scenarioLandscape . scenarioCosmetics) . fst) $ c ^. gameplay . scenarioRef
    nameWidget = WithWidth (2 + T.length nameTxt) w
     where
      w =
        hBox
          [ renderDisplay aMap (r ^. robotDisplay)
          , highlightSystem . txt $ " " <> nameTxt
          ]
      nameTxt = r ^. robotName

    highlightSystem = applyWhen (r ^. systemRobot) $ withAttr highlightAttr

    ageStr
      | age < 60 = show age <> "sec"
      | age < 3600 = show (age `div` 60) <> "min"
      | age < 3600 * 24 = show (age `div` 3600) <> "hour"
      | otherwise = show (age `div` 3600 * 24) <> "day"
     where
      TimeSpec createdAtSec _ = r ^. robotCreatedAt
      TimeSpec nowSec _ = c ^. timing . lastFrameTime
      age = nowSec - createdAtSec

    rInvCount = sum $ map fst . E.elems $ r ^. robotEntity . entityInventory
    rLog
      | r ^. robotLogUpdated = 'x'
      | otherwise = ' '

    locWidget =
      WithWidth (2 + length locStr) w
     where
      w = hBox [worldCell, str $ " " <> locStr]
      rCoords = fmap locToCoords rLoc
      rLoc = r ^. robotLocation
      worldCell =
        drawLoc
          (c ^. gameplay)
          g
          rCoords
      locStr = renderCoordsString rLoc

    statusWidget = case r ^. machine of
      Waiting {} -> strWidget "waiting"
      _
        | isActive r -> withAttr notifAttr <$> strWidget "busy"
        | otherwise -> withAttr greenAttr <$> strWidget "idle"

  basePos :: Point V2 Double
  basePos = realToFrac <$> fromMaybe origin (g ^? baseRobot . robotLocation . planar)
  -- Keep the base and non system robot (e.g. no seed)
  isRelevant r = r ^. robotID == 0 || not (r ^. systemRobot)
  -- Keep the robot that are less than 32 unit away from the base
  isNear r = creative || distance (realToFrac <$> r ^. robotLocation . planar) basePos < 32
  robots :: [Robot]
  robots =
    filter (\r -> debugAllRobots || (isRelevant r && isNear r))
      . IM.elems
      $ g ^. robotInfo . robotMap
  creative = g ^. creativeMode
  debugAllRobots = c ^. uiDbg . Lens.contains ListAllRobots
  g = c ^. mygs

drawRobotsModal :: RobotDisplay -> Widget Name
drawRobotsModal robotDialog =
  mainContent
 where
  rFocusRing = robotDialog ^. robotDetailsFocus

  mainContent =
    if robotDialog ^. isDetailsOpened
      then
        let oldList = getList $ robotDialog ^. robotListContent . robotsListWidget
            maybeSelectedRobot = view robot . snd <$> BL.listSelectedElement oldList
            detailsContent = case maybeSelectedRobot of
              Nothing -> str "No selection"
              Just r -> renderRobotDetails rFocusRing r $ robotDialog ^. robotListContent . robotDetailsPaneState
         in vBox
              [ detailsContent
              , tabControlFooter
              ]
      else renderRobotsList $ robotDialog ^. robotListContent
