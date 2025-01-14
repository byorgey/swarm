{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Code for drawing the TUI.
module Swarm.TUI.View (
  drawUI,
  drawTPS,

  -- * Dialog box
  drawDialog,
  chooseCursor,

  -- * Key hint menu
  drawKeyMenu,
  drawModalMenu,
  drawKeyCmd,

  -- * Robot panel
  drawRobotPanel,
  drawItem,
  renderDutyCycle,

  -- * Info panel
  drawInfoPanel,
  explainFocusedItem,

  -- * REPL
  drawREPL,
) where

import Brick hiding (Direction, Location)
import Brick.Focus
import Brick.Forms
import Brick.Widgets.Border (
  hBorder,
  hBorderWithLabel,
  joinableBorder,
  vBorder,
 )
import Brick.Widgets.Center (center, centerLayer, hCenter)
import Brick.Widgets.Dialog
import Brick.Widgets.Edit (getEditContents, renderEditor)
import Brick.Widgets.List qualified as BL
import Brick.Widgets.Table qualified as BT
import Control.Lens as Lens hiding (Const, from)
import Control.Monad (guard)
import Data.Array (range)
import Data.Bits (shiftL, shiftR, (.&.))
import Data.Foldable (toList)
import Data.Foldable qualified as F
import Data.Functor (($>))
import Data.List (intersperse)
import Data.List qualified as L
import Data.List.Extra (enumerate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.List.Split (chunksOf)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe, maybeToList)
import Data.Semigroup (sconcat)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, defaultTimeLocale, formatTime)
import Network.Wai.Handler.Warp (Port)
import Swarm.Constant
import Swarm.Game.Device (commandCost, commandsForDeviceCaps, enabledCommands, getCapabilitySet, getMap, ingredients)
import Swarm.Game.Display
import Swarm.Game.Entity as E
import Swarm.Game.Entity.Cosmetic
import Swarm.Game.Ingredients
import Swarm.Game.Land
import Swarm.Game.Location
import Swarm.Game.Recipe
import Swarm.Game.Robot
import Swarm.Game.Robot.Concrete
import Swarm.Game.Scenario (
  scenarioAuthor,
  scenarioCosmetics,
  scenarioCreative,
  scenarioDescription,
  scenarioKnown,
  scenarioLandscape,
  scenarioMetadata,
  scenarioName,
  scenarioObjectives,
  scenarioOperation,
  scenarioSeed,
  scenarioTerrainAndEntities,
 )
import Swarm.Game.Scenario.Scoring.Best
import Swarm.Game.Scenario.Scoring.CodeSize
import Swarm.Game.Scenario.Scoring.ConcreteMetrics
import Swarm.Game.Scenario.Scoring.GenericMetrics
import Swarm.Game.Scenario.Status
import Swarm.Game.Scenario.Topography.Center
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.ScenarioInfo (
  ScenarioItem (..),
  scenarioItemName,
 )
import Swarm.Game.State
import Swarm.Game.State.Landscape
import Swarm.Game.State.Robot
import Swarm.Game.State.Runtime
import Swarm.Game.State.Substate
import Swarm.Game.Tick (TickNumber (..))
import Swarm.Game.Universe
import Swarm.Game.World.Coords
import Swarm.Game.World.Gen (Seed)
import Swarm.Language.Capability (Capability (..), constCaps)
import Swarm.Language.Syntax
import Swarm.Language.Typecheck (inferConst)
import Swarm.Log
import Swarm.Pretty (prettyText, prettyTextLine, prettyTextWidth)
import Swarm.TUI.Border
import Swarm.TUI.Controller (ticksPerFrameCap)
import Swarm.TUI.Controller.EventHandlers (allEventHandlers, mainEventHandlers, replEventHandlers, robotEventHandlers, worldEventHandlers)
import Swarm.TUI.Controller.Util (hasDebugCapability)
import Swarm.TUI.Editor.Model
import Swarm.TUI.Editor.View qualified as EV
import Swarm.TUI.Inventory.Sorting (renderSortMethod)
import Swarm.TUI.Launch.Model
import Swarm.TUI.Launch.View
import Swarm.TUI.Model
import Swarm.TUI.Model.DebugOption (DebugOption (..))
import Swarm.TUI.Model.Dialog.Goal (goalsContent, hasAnythingToShow)
import Swarm.TUI.Model.Event qualified as SE
import Swarm.TUI.Model.KeyBindings (handlerNameKeysDescription)
import Swarm.TUI.Model.Repl
import Swarm.TUI.Model.UI
import Swarm.TUI.Model.UI.Gameplay
import Swarm.TUI.Panel
import Swarm.TUI.View.Achievement
import Swarm.TUI.View.Attribute.Attr
import Swarm.TUI.View.CellDisplay
import Swarm.TUI.View.Logo
import Swarm.TUI.View.Objective qualified as GR
import Swarm.TUI.View.Popup
import Swarm.TUI.View.Robot
import Swarm.TUI.View.Structure qualified as SR
import Swarm.TUI.View.Util as VU
import Swarm.Util
import Text.Printf
import Text.Wrap
import Witch (into)

-- | The main entry point for drawing the entire UI.
drawUI :: AppState -> [Widget Name]
drawUI s = drawPopups s : mainLayers
 where
  mainLayers
    | s ^. uiState . uiPlaying = drawGameUI s
    | otherwise = drawMenuUI s

drawMenuUI :: AppState -> [Widget Name]
drawMenuUI s = case s ^. uiState . uiMenu of
  -- We should never reach the NoMenu case if uiPlaying is false; we would have
  -- quit the app instead.  But just in case, we display the main menu anyway.
  NoMenu -> [drawMainMenuUI s (mainMenu NewGame)]
  MainMenu l -> [drawMainMenuUI s l]
  NewGameMenu stk -> drawNewGameMenuUI stk $ s ^. uiState . uiLaunchConfig
  AchievementsMenu l -> [drawAchievementsMenuUI s l]
  MessagesMenu -> [drawMainMessages s]
  AboutMenu -> [drawAboutMenuUI (s ^. runtimeState . appData . at "about")]

drawMainMessages :: AppState -> Widget Name
drawMainMessages s = renderDialog dial . padBottom Max . scrollList $ drawLogs ls
 where
  ls = reverse $ s ^. runtimeState . eventLog . notificationsContent
  dial = dialog (Just $ str "Messages") Nothing maxModalWindowWidth
  scrollList = withVScrollBars OnRight . vBox
  drawLogs = map (drawLogEntry True)

drawMainMenuUI :: AppState -> BL.List Name MainMenuEntry -> Widget Name
drawMainMenuUI s l =
  vBox . catMaybes $
    [ drawLogo <$> logo
    , hCenter . padTopBottom 2 <$> newVersionWidget version
    , Just . centerLayer . vLimit 6 . hLimit 20 $
        BL.renderList (const (hCenter . drawMainMenuEntry s)) True l
    ]
 where
  logo = s ^. runtimeState . appData . at "logo"
  version = s ^. runtimeState . upstreamRelease

newVersionWidget :: Either (Severity, Text) String -> Maybe (Widget n)
newVersionWidget = \case
  Right ver -> Just . txt $ "New version " <> T.pack ver <> " is available!"
  Left _ -> Nothing

-- | When launching a game, a modal prompt may appear on another layer
-- to input seed and/or a script to run.
drawNewGameMenuUI ::
  NonEmpty (BL.List Name ScenarioItem) ->
  LaunchOptions ->
  [Widget Name]
drawNewGameMenuUI (l :| ls) launchOptions = case displayedFor of
  Nothing -> pure mainWidget
  Just _ -> drawLaunchConfigPanel launchOptions <> pure mainWidget
 where
  displayedFor = launchOptions ^. controls . isDisplayedFor
  mainWidget =
    vBox
      [ padLeftRight 20
          . centerLayer
          $ hBox
            [ vBox
                [ withAttr boldAttr . txt $ breadcrumbs ls
                , txt " "
                , vLimit 20
                    . hLimit 35
                    . withLeftPaddedVScrollBars
                    . padLeft (Pad 1)
                    . padTop (Pad 1)
                    . BL.renderList (const $ padRight Max . drawScenarioItem) True
                    $ l
                ]
            , padLeft (Pad 5) (maybe (txt "") (drawDescription . snd) (BL.listSelectedElement l))
            ]
      , launchOptionsMessage
      ]

  launchOptionsMessage = case (displayedFor, snd <$> BL.listSelectedElement l) of
    (Nothing, Just (SISingle _)) -> hCenter $ txt "Press 'o' for launch options, or 'Enter' to launch with defaults"
    _ -> txt " "

  drawScenarioItem (SISingle (s, si)) = padRight (Pad 1) (drawStatusInfo s si) <+> txt (s ^. scenarioMetadata . scenarioName)
  drawScenarioItem (SICollection nm _) = padRight (Pad 1) (withAttr boldAttr $ txt " > ") <+> txt nm
  drawStatusInfo s si = case si ^. scenarioStatus of
    NotStarted -> txt " ○ "
    Played _initialScript (Metric Attempted _) _ -> case s ^. scenarioOperation . scenarioObjectives of
      [] -> withAttr cyanAttr $ txt " ◉ "
      _ -> withAttr yellowAttr $ txt " ◎ "
    Played _initialScript (Metric Completed _) _ -> withAttr greenAttr $ txt " ● "

  describeStatus :: ScenarioStatus -> Widget n
  describeStatus = \case
    NotStarted -> withAttr cyanAttr $ txt "not started"
    Played _initialScript pm _best -> describeProgress pm

  breadcrumbs :: [BL.List Name ScenarioItem] -> Text
  breadcrumbs =
    T.intercalate " > "
      . ("Scenarios" :)
      . reverse
      . mapMaybe (fmap (scenarioItemName . snd) . BL.listSelectedElement)

  drawDescription :: ScenarioItem -> Widget Name
  drawDescription (SICollection _ _) = txtWrap " "
  drawDescription (SISingle (s, si)) =
    vBox
      [ drawMarkdown (nonBlank (s ^. scenarioOperation . scenarioDescription))
      , cached (ScenarioPreview $ si ^. scenarioPath) $
          hCenter . padTop (Pad 1) . vLimit 6 $
            hLimitPercent 60 worldPeek
      , padTop (Pad 1) table
      ]
   where
    vc = determineStaticViewCenter (s ^. scenarioLandscape) worldTuples

    worldTuples = buildWorldTuples $ s ^. scenarioLandscape
    theWorlds =
      genMultiWorld worldTuples $
        fromMaybe 0 $
          s ^. scenarioLandscape . scenarioSeed

    entIsKnown =
      getEntityIsKnown $
        EntityKnowledgeDependencies
          { isCreativeMode = s ^. scenarioOperation . scenarioCreative
          , globallyKnownEntities = s ^. scenarioLandscape . scenarioKnown
          , theFocusedRobot = Nothing
          }

    tm = s ^. scenarioLandscape . scenarioTerrainAndEntities . terrainMap
    ri = RenderingInput theWorlds entIsKnown tm

    aMap = s ^. scenarioLandscape . scenarioCosmetics

    renderCoord = renderDisplay aMap . displayLocRaw (WorldOverdraw False mempty) ri []
    worldPeek = worldWidget renderCoord vc

    firstRow =
      ( withAttr dimAttr $ txt "Author:"
      , withAttr dimAttr . txt <$> s ^. scenarioMetadata . scenarioAuthor
      )
    secondRow =
      ( txt "last:"
      , Just $ describeStatus $ si ^. scenarioStatus
      )

    padTopLeft = padTop (Pad 1) . padLeft (Pad 1)

    tableRows =
      map (map padTopLeft . pairToList) $
        mapMaybe sequenceA $
          firstRow : secondRow : makeBestScoreRows (si ^. scenarioStatus)
    table =
      BT.renderTable
        . BT.surroundingBorder False
        . BT.rowBorders False
        . BT.columnBorders False
        . BT.alignRight 0
        . BT.alignLeft 1
        . BT.table
        $ tableRows

  nonBlank "" = " "
  nonBlank t = t

pairToList :: (a, a) -> [a]
pairToList (x, y) = [x, y]

describeProgress :: ProgressMetric -> Widget n
describeProgress (Metric p (ProgressStats _startedAt (AttemptMetrics (DurationMetrics e t) maybeCodeMetrics))) = case p of
  Attempted ->
    withAttr yellowAttr . vBox $
      [ txt "in progress"
      , txt $ parens $ T.unwords ["played for", formatTimeDiff e]
      ]
  Completed ->
    withAttr greenAttr . vBox $
      [ txt $ T.unwords ["completed in", formatTimeDiff e]
      , txt . (" " <>) . parens $ T.unwords [T.pack $ drawTime t True, "ticks"]
      ]
        <> maybeToList (sizeDisplay <$> maybeCodeMetrics)
   where
    sizeDisplay (ScenarioCodeMetrics myCharCount myAstSize) =
      withAttr greenAttr $
        vBox $
          map
            txt
            [ T.unwords
                [ "Code:"
                , T.pack $ show myCharCount
                , "chars"
                ]
            , (" " <>) $
                parens $
                  T.unwords
                    [ T.pack $ show myAstSize
                    , "AST nodes"
                    ]
            ]
 where
  formatTimeDiff :: NominalDiffTime -> Text
  formatTimeDiff = T.pack . formatTime defaultTimeLocale "%hh %Mm %Ss"

-- | If there are multiple different games that each are \"best\"
-- by different criteria, display them all separately, labeled
-- by which criteria they were best in.
--
-- On the other hand, if all of the different \"best\" criteria are for the
-- same game, consolidate them all into one entry and don't bother
-- labelling the criteria.
makeBestScoreRows ::
  ScenarioStatus ->
  [(Widget n1, Maybe (Widget n2))]
makeBestScoreRows scenarioStat =
  maybe [] makeBestRows getBests
 where
  getBests = case scenarioStat of
    NotStarted -> Nothing
    Played _initialScript _ best -> Just best

  makeBestRows b = map (makeBestRow hasMultiple) groups
   where
    groups = getBestGroups b
    hasMultiple = length groups > 1

  makeBestRow hasDistinctByCriteria (b, criteria) =
    ( hLimit (maxLeftColumnWidth + 2) $
        vBox $
          [ padLeft Max $ txt "best:"
          ]
            <> elaboratedCriteria
    , Just $ describeProgress b
    )
   where
    maxLeftColumnWidth = maximum0 (map (T.length . describeCriteria) enumerate)
    mkCriteriaRow =
      withAttr dimAttr
        . padLeft Max
        . txt
        . mconcat
        . pairToList
        . fmap (\x -> T.singleton $ if x == 0 then ',' else ' ')
    elaboratedCriteria =
      if hasDistinctByCriteria
        then
          map mkCriteriaRow
            . flip zip [(0 :: Int) ..]
            . NE.toList
            . NE.reverse
            . NE.map describeCriteria
            $ criteria
        else []

drawMainMenuEntry :: AppState -> MainMenuEntry -> Widget Name
drawMainMenuEntry s = \case
  NewGame -> txt "New game"
  Tutorial -> txt "Tutorial"
  Achievements -> txt "Achievements"
  About -> txt "About"
  Messages -> highlightMessages $ txt "Messages"
  Quit -> txt "Quit"
 where
  highlightMessages =
    applyWhen (s ^. runtimeState . eventLog . notificationsCount > 0) $
      withAttr notifAttr

drawAboutMenuUI :: Maybe Text -> Widget Name
drawAboutMenuUI Nothing = centerLayer $ txt "About swarm!"
drawAboutMenuUI (Just t) = centerLayer . vBox . map (hCenter . txt . nonblank) $ T.lines t
 where
  -- Turn blank lines into a space so they will take up vertical space as widgets
  nonblank "" = " "
  nonblank s = s

-- | Draw the main game UI.  Generates a list of widgets, where each
--   represents a layer.  Right now we just generate two layers: the
--   main layer and a layer for a floating dialog that can be on top.
drawGameUI :: AppState -> [Widget Name]
drawGameUI s =
  [ joinBorders $ drawDialog s
  , joinBorders $
      hBox
        [ hLimitPercent 25 $
            vBox
              [ vLimitPercent 50
                  $ panel
                    highlightAttr
                    fr
                    (FocusablePanel RobotPanel)
                    ( plainBorder
                        & bottomLabels . centerLabel
                          .~ fmap
                            (txt . (" Search: " <>) . (<> " "))
                            (uig ^. uiInventory . uiInventorySearch)
                    )
                  $ drawRobotPanel s
              , panel
                  highlightAttr
                  fr
                  (FocusablePanel InfoPanel)
                  plainBorder
                  $ drawInfoPanel s
              , hCenter
                  . clickable (FocusablePanel WorldEditorPanel)
                  . EV.drawWorldEditor fr
                  $ uig
              ]
        , vBox rightPanel
        ]
  ]
 where
  uig = s ^. uiState . uiGameplay
  addCursorPos = bottomLabels . leftLabel ?~ padLeftRight 1 widg
   where
    widg = case uig ^. uiWorldCursor of
      Nothing -> str $ renderCoordsString $ s ^. gameState . robotInfo . viewCenter
      Just coord ->
        clickable WorldPositionIndicator $
          drawWorldCursorInfo
            aMap
            (uig ^. uiWorldEditor . worldOverdraw)
            (s ^. gameState)
            coord
       where
        aMap =
          maybe mempty (view (scenarioLandscape . scenarioCosmetics) . fst) $
            s ^. uiState . uiGameplay . scenarioRef

  -- Add clock display in top right of the world view if focused robot
  -- has a clock equipped
  addClock = topLabels . rightLabel ?~ padLeftRight 1 (drawClockDisplay (uig ^. uiTiming . lgTicksPerSecond) $ s ^. gameState)
  fr = uig ^. uiFocusRing
  showREPL = uig ^. uiShowREPL
  rightPanel = if showREPL then worldPanel ++ replPanel else worldPanel ++ minimizedREPL
  minimizedREPL = case focusGetCurrent fr of
    (Just (FocusablePanel REPLPanel)) -> [separateBorders $ clickable (FocusablePanel REPLPanel) (forceAttr highlightAttr hBorder)]
    _ -> [separateBorders $ clickable (FocusablePanel REPLPanel) hBorder]
  worldPanel =
    [ panel
        highlightAttr
        fr
        (FocusablePanel WorldPanel)
        ( plainBorder
            & bottomLabels . rightLabel ?~ padLeftRight 1 (drawTPS $ uig ^. uiTiming)
            & topLabels . leftLabel ?~ drawModalMenu s
            & addCursorPos
            & addClock
        )
        (drawWorldPane uig (s ^. gameState))
    , drawKeyMenu s
    ]
  replPanel =
    [ clickable (FocusablePanel REPLPanel) $
        panel
          highlightAttr
          fr
          (FocusablePanel REPLPanel)
          ( plainBorder
              & topLabels . rightLabel .~ (drawType <$> (uig ^. uiREPL . replType))
          )
          ( vLimit replHeight
              . padBottom Max
              . padLeft (Pad 1)
              $ drawREPL s
          )
    ]

drawWorldCursorInfo ::
  M.Map WorldAttr PreservableColor ->
  WorldOverdraw ->
  GameState ->
  Cosmic Coords ->
  Widget Name
drawWorldCursorInfo aMap worldEditor g cCoords =
  case getStatic g coords of
    Just s -> renderDisplay aMap $ displayStatic s
    Nothing -> hBox $ tileMemberWidgets ++ [coordsWidget]
 where
  Cosmic _ coords = cCoords
  coordsWidget = str $ renderCoordsString $ fmap coordsToLoc cCoords

  tileMembers = terrain : mapMaybe merge [entity, r]
  tileMemberWidgets =
    map (padRight $ Pad 1)
      . concat
      . reverse
      . zipWith f tileMembers
      $ ["at", "on", "with"]
   where
    f cell preposition = [renderDisplay aMap cell, txt preposition]

  ri =
    RenderingInput
      (g ^. landscape . multiWorld)
      (getEntityIsKnown $ mkEntityKnowledge g)
      (g ^. landscape . terrainAndEntities . terrainMap)

  terrain = displayTerrainCell worldEditor ri cCoords
  entity = displayEntityCell worldEditor ri cCoords
  r = displayRobotCell g cCoords

  merge = fmap sconcat . NE.nonEmpty . filter (not . (^. invisible))

-- | Format the clock display to be shown in the upper right of the
--   world panel.
drawClockDisplay :: Int -> GameState -> Widget n
drawClockDisplay lgTPS gs = hBox . intersperse (txt " ") $ catMaybes [clockWidget, pauseWidget]
 where
  clockWidget = maybeDrawTime (gs ^. temporal . ticks) (gs ^. temporal . paused || lgTPS < 3) gs
  pauseWidget = guard (gs ^. temporal . paused) $> txt "(PAUSED)"

-- | Check whether the currently focused robot (if any) has some kind
-- of a clock device equipped.
clockEquipped :: GameState -> Bool
clockEquipped gs = case focusedRobot gs of
  Nothing -> False
  Just r -> CExecute Time `Set.member` getCapabilitySet (r ^. robotCapabilities)

-- | Format a ticks count as a hexadecimal clock.
drawTime :: TickNumber -> Bool -> String
drawTime (TickNumber t) showTicks =
  mconcat $
    intersperse
      ":"
      [ printf "%x" (t `shiftR` 20)
      , printf "%02x" ((t `shiftR` 12) .&. ((1 `shiftL` 8) - 1))
      , printf "%02x" ((t `shiftR` 4) .&. ((1 `shiftL` 8) - 1))
      ]
      ++ if showTicks then [".", printf "%x" (t .&. ((1 `shiftL` 4) - 1))] else []

-- | Return a possible time display, if the currently focused robot
--   has a clock device equipped.  The first argument is the number
--   of ticks (e.g. 943 = 0x3af), and the second argument indicates
--   whether the time should be shown down to single-tick resolution
--   (e.g. 0:00:3a.f) or not (e.g. 0:00:3a).
maybeDrawTime :: TickNumber -> Bool -> GameState -> Maybe (Widget n)
maybeDrawTime t showTicks gs = guard (clockEquipped gs) $> str (drawTime t showTicks)

-- | Draw info about the current number of ticks per second.
drawTPS :: UITiming -> Widget Name
drawTPS timing = hBox (tpsInfo : rateInfo)
 where
  tpsInfo
    | l >= 0 = hBox [str (show n), txt " ", txt (number n "tick"), txt " / s"]
    | otherwise = hBox [txt "1 tick / ", str (show n), txt " s"]

  rateInfo
    | timing ^. uiShowFPS =
        [ txt " ("
        , let tpf = timing ^. uiTPF
           in applyWhen (tpf >= fromIntegral ticksPerFrameCap) (withAttr redAttr) $
                str (printf "%0.1f" tpf)
        , txt " tpf, "
        , str (printf "%0.1f" (timing ^. uiFPS))
        , txt " fps)"
        ]
    | otherwise = []

  l = timing ^. lgTicksPerSecond
  n = 2 ^ abs l

-- | The height of the REPL box.  Perhaps in the future this should be
--   configurable.
replHeight :: Int
replHeight = 10

-- | Hide the cursor when a modal is set
chooseCursor :: AppState -> [CursorLocation n] -> Maybe (CursorLocation n)
chooseCursor s locs = case m of
  Nothing -> showFirstCursor s locs
  Just _ -> Nothing
 where
  m = s ^. uiState . uiGameplay . uiDialogs . uiModal

-- | Draw a dialog window, if one should be displayed right now.
drawDialog :: AppState -> Widget Name
drawDialog s = case m of
  Just (Modal mt d) -> renderDialog d $ case mt of
    GoalModal -> drawModal s mt
    RobotsModal -> drawModal s mt
    _ -> maybeScroll ModalViewport $ drawModal s mt
  Nothing -> emptyWidget
 where
  m = s ^. uiState . uiGameplay . uiDialogs . uiModal

-- | Draw one of the various types of modal dialog.
drawModal :: AppState -> ModalType -> Widget Name
drawModal s = \case
  HelpModal ->
    helpWidget
      (gs ^. randomness . seed)
      (s ^. runtimeState . webPort)
      (s ^. keyEventHandling)
  RobotsModal -> drawRobotsModal $ uig ^. uiDialogs . uiRobot
  RecipesModal -> availableListWidget gs RecipeList
  CommandsModal -> commandsListWidget (s ^. uiState . uiGameplay) gs
  MessagesModal -> availableListWidget gs MessageList
  StructuresModal -> SR.renderStructuresDisplay gs uig
  ScenarioEndModal outcome ->
    padBottom (Pad 1) $
      vBox $
        map
          (hCenter . txt)
          content
   where
    content = case outcome of
      WinModal -> ["Congratulations!"]
      LoseModal ->
        [ "Condolences!"
        , "This scenario is no longer winnable."
        ]
  DescriptionModal e -> descriptionWidget s e
  QuitModal -> padBottom (Pad 1) $ hCenter $ txt (quitMsg (s ^. uiState . uiMenu))
  GoalModal ->
    GR.renderGoalsDisplay (uig ^. uiDialogs . uiGoal) $
      view (scenarioOperation . scenarioDescription) . fst <$> uig ^. scenarioRef
  KeepPlayingModal ->
    padLeftRight 1 $
      displayParagraphs $
        pure
          "Have fun!  Hit Ctrl-Q whenever you're ready to proceed to the next challenge or return to the menu."
  TerrainPaletteModal -> EV.drawTerrainSelector uig
  EntityPaletteModal -> EV.drawEntityPaintSelector uig
 where
  gs = s ^. gameState
  uig = s ^. uiState . uiGameplay

helpWidget :: Seed -> Maybe Port -> KeyEventHandlingState -> Widget Name
helpWidget theSeed mport keyState =
  padLeftRight 2 . vBox $
    padTop (Pad 1)
      <$> [ info
          , colorizationLegend
          , helpKeys
          , tips
          ]
 where
  tips =
    vBox
      [ heading boldAttr "Have questions? Want some tips? Check out:"
      , txt "  - The Swarm wiki, " <+> hyperlink wikiUrl (txt wikiUrl)
      , txt "  - The Swarm Discord server at " <+> hyperlink swarmDiscord (txt swarmDiscord)
      ]
  info =
    vBox
      [ heading boldAttr "Configuration"
      , txt ("Seed: " <> into @Text (show theSeed))
      , txt ("Web server port: " <> maybe "none" (into @Text . show) mport)
      ]
  colorizationLegend =
    vBox
      [ heading boldAttr "Colorization legend"
      , drawMarkdown
          "In text, snippets of code like `3 + 4` or `scan down` will be colorized. Types like `Cmd Text`{=type} have a dedicated color. The names of an `entity`{=entity}, a `structure`{=structure}, and a `tag`{=tag} also each have their own color."
      ]
  helpKeys =
    vBox
      [ heading boldAttr "Keybindings"
      , keySection "Main (always active)" mainEventHandlers
      , keySection "REPL panel" replEventHandlers
      , keySection "World view panel" worldEventHandlers
      , keySection "Robot inventory panel" robotEventHandlers
      ]
  keySection name handlers =
    padBottom (Pad 1) $
      vBox
        [ heading italicAttr name
        , mkKeyTable handlers
        ]
  mkKeyTable =
    BT.renderTable
      . BT.surroundingBorder False
      . BT.rowBorders False
      . BT.table
      . map (toRow . keyHandlerToText)
  heading attr = padBottom (Pad 1) . withAttr attr . txt
  toRow (n, k, d) =
    [ padRight (Pad 1) $ txtFilled maxN n
    , padLeftRight 1 $ txtFilled maxK k
    , padLeft (Pad 1) $ txtFilled maxD d
    ]
  keyHandlerToText = handlerNameKeysDescription (keyState ^. keyConfig)
  -- Get maximum width of the table columns so it all neatly aligns
  txtFilled n t = padRight (Pad $ max 0 (n - textWidth t)) $ txt t
  (maxN, maxK, maxD) = map3 (maximum0 . map textWidth) . unzip3 $ keyHandlerToText <$> allEventHandlers
  map3 f (n, k, d) = (f n, f k, f d)

data NotificationList = RecipeList | MessageList

availableListWidget :: GameState -> NotificationList -> Widget Name
availableListWidget gs nl = padTop (Pad 1) $ vBox widgetList
 where
  widgetList = case nl of
    RecipeList -> mkAvailableList gs (discovery . availableRecipes) renderRecipe
    MessageList -> messagesWidget gs
  renderRecipe = padLeftRight 18 . drawRecipe Nothing (fromMaybe E.empty inv)
  inv = gs ^? to focusedRobot . _Just . robotInventory

mkAvailableList :: GameState -> Lens' GameState (Notifications a) -> (a -> Widget Name) -> [Widget Name]
mkAvailableList gs notifLens notifRender = map padRender news <> notifSep <> map padRender knowns
 where
  padRender = padBottom (Pad 1) . notifRender
  count = gs ^. notifLens . notificationsCount
  (news, knowns) = splitAt count (gs ^. notifLens . notificationsContent)
  notifSep
    | count > 0 && not (null knowns) =
        [ padBottom (Pad 1) (withAttr redAttr $ hBorderWithLabel (padLeftRight 1 (txt "new↑")))
        ]
    | otherwise = []

commandsListWidget :: UIGameplay -> GameState -> Widget Name
commandsListWidget uig gs =
  hCenter $
    vBox
      [ table
      , padTop (Pad 1) $ txt "For the full list of available commands see the Wiki at:"
      , txt wikiCheatSheet
      ]
 where
  commands = gs ^. discovery . availableCommands . notificationsContent
  table =
    BT.renderTable
      . BT.surroundingBorder False
      . BT.columnBorders False
      . BT.rowBorders False
      . BT.setDefaultColAlignment BT.AlignLeft
      . BT.alignRight 0
      . BT.table
      $ headers : commandsTable
  headers =
    withAttr robotAttr
      <$> [ txt "command name"
          , txt " : type"
          , txt "Enabled by"
          ]

  commandsTable = mkCmdRow <$> commands
  mkCmdRow cmd =
    map
      (padTop $ Pad 1)
      [ txt $ syntax $ constInfo cmd
      , padRight (Pad 2) . withAttr magentaAttr . txt $ " : " <> prettyTextLine (inferConst cmd)
      , listDevices cmd
      ]

  base = gs ^? baseRobot
  entsByCap = case base of
    Just r ->
      M.map NE.toList $
        entitiesByCapability $
          (r ^. equippedDevices) `union` (r ^. robotInventory)
    Nothing -> mempty

  aMap = maybe mempty (view (scenarioLandscape . scenarioCosmetics) . fst) $ uig ^. scenarioRef
  listDevices cmd = vBox $ map (drawLabelledEntityName aMap) providerDevices
   where
    providerDevices =
      concatMap (flip (M.findWithDefault []) entsByCap) $
        maybeToList $
          constCaps cmd

-- | Generate a pop-up widget to display the description of an entity.
descriptionWidget :: AppState -> Entity -> Widget Name
descriptionWidget s e = padLeftRight 1 (explainEntry uig gs e)
 where
  gs = s ^. gameState
  uig = s ^. uiState . uiGameplay

-- | Draw a widget with messages to the current robot.
messagesWidget :: GameState -> [Widget Name]
messagesWidget gs = widgetList
 where
  widgetList = focusNewest . map drawLogEntry' $ gs ^. messageNotifications . notificationsContent
  focusNewest = applyWhen (not $ gs ^. temporal . paused) $ over _last visible
  drawLogEntry' e =
    withAttr (colorLogs e) $
      hBox
        [ fromMaybe (txt "") $ maybeDrawTime (e ^. leTime) True gs
        , padLeft (Pad 2) . txt $ brackets $ e ^. leName
        , padLeft (Pad 1) . txt2 $ e ^. leText
        ]
  txt2 = txtWrapWith indent2

colorLogs :: LogEntry -> AttrName
colorLogs e = case e ^. leSource of
  SystemLog -> colorSeverity (e ^. leSeverity)
  RobotLog rls rid _loc -> case rls of
    Said -> robotColor rid
    Logged -> notifAttr
    RobotError -> colorSeverity (e ^. leSeverity)
    CmdStatus -> notifAttr
 where
  -- color each robot message with different color of the world
  robotColor = indexWrapNonEmpty messageAttributeNames

colorSeverity :: Severity -> AttrName
colorSeverity = \case
  Info -> infoAttr
  Debug -> dimAttr
  Warning -> yellowAttr
  Error -> redAttr
  Critical -> redAttr

-- | Draw the F-key modal menu. This is displayed in the top left world corner.
drawModalMenu :: AppState -> Widget Name
drawModalMenu s = vLimit 1 . hBox $ map (padLeftRight 1 . drawKeyCmd) globalKeyCmds
 where
  notificationKey :: Getter GameState (Notifications a) -> SE.MainEvent -> Text -> Maybe (KeyHighlight, Text, Text)
  notificationKey notifLens key name
    | null (s ^. gameState . notifLens . notificationsContent) = Nothing
    | otherwise =
        let highlight
              | s ^. gameState . notifLens . notificationsCount > 0 = Alert
              | otherwise = NoHighlight
         in Just (highlight, keyM key, name)

  -- Hides this key if the recognizable structure list is empty
  structuresKey =
    if null $ s ^. gameState . landscape . recognizerAutomatons . originalStructureDefinitions
      then Nothing
      else Just (NoHighlight, keyM SE.ViewStructuresEvent, "Structures")

  globalKeyCmds =
    catMaybes
      [ Just (NoHighlight, keyM SE.ViewHelpEvent, "Help")
      , Just (NoHighlight, keyM SE.ViewRobotsEvent, "Robots")
      , notificationKey (discovery . availableRecipes) SE.ViewRecipesEvent "Recipes"
      , notificationKey (discovery . availableCommands) SE.ViewCommandsEvent "Commands"
      , notificationKey messageNotifications SE.ViewMessagesEvent "Messages"
      , structuresKey
      ]
  keyM = VU.bindingText s . SE.Main

-- | Draw a menu explaining what key commands are available for the
--   current panel.  This menu is displayed as one or two lines in
--   between the world panel and the REPL.
--
-- This excludes the F-key modals that are shown elsewhere.
drawKeyMenu :: AppState -> Widget Name
drawKeyMenu s =
  vLimit 2 $
    hBox
      [ padBottom Max $
          vBox
            [ mkCmdRow globalKeyCmds
            , padLeft (Pad 2) contextCmds
            ]
      , gameModeWidget
      ]
 where
  mkCmdRow = hBox . map drawPaddedCmd
  drawPaddedCmd = padLeftRight 1 . drawKeyCmd
  contextCmds
    | ctrlMode == Handling = txt $ fromMaybe "" (gs ^? gameControls . inputHandler . _Just . _1)
    | otherwise = mkCmdRow focusedPanelCmds
  focusedPanelCmds =
    map highlightKeyCmds
      . keyCmdsFor
      . focusGetCurrent
      . view uiFocusRing
      $ uig

  uig = s ^. uiState . uiGameplay
  gs = s ^. gameState

  isReplWorking = gs ^. gameControls . replWorking
  isPaused = gs ^. temporal . paused
  hasDebug = hasDebugCapability creative s
  viewingBase = (gs ^. robotInfo . viewCenterRule) == VCRobot 0
  creative = gs ^. creativeMode
  showCreative = s ^. uiState . uiDebugOptions . Lens.contains ToggleCreative
  showEditor = s ^. uiState . uiDebugOptions . Lens.contains ToggleWorldEditor
  goal = hasAnythingToShow $ uig ^. uiDialogs . uiGoal . goalsContent
  showZero = uig ^. uiInventory . uiShowZero
  inventorySort = uig ^. uiInventory . uiInventorySort
  inventorySearch = uig ^. uiInventory . uiInventorySearch
  ctrlMode = uig ^. uiREPL . replControlMode
  canScroll = creative || (gs ^. landscape . worldScrollable)
  handlerInstalled = isJust (gs ^. gameControls . inputHandler)

  renderPilotModeSwitch :: ReplControlMode -> T.Text
  renderPilotModeSwitch = \case
    Piloting -> "REPL"
    _ -> "pilot"

  renderHandlerModeSwitch :: ReplControlMode -> T.Text
  renderHandlerModeSwitch = \case
    Handling -> "REPL"
    _ -> "key handler"

  gameModeWidget =
    padLeft Max
      . padLeftRight 1
      . txt
      . (<> " mode")
      $ case creative of
        False -> "Classic"
        True -> "Creative"
  globalKeyCmds =
    catMaybes
      [ may goal (NoHighlight, keyM SE.ViewGoalEvent, "goal")
      , may showCreative (NoHighlight, keyM SE.ToggleCreativeModeEvent, "creative")
      , may showEditor (NoHighlight, keyM SE.ToggleWorldEditorEvent, "editor")
      , Just (NoHighlight, keyM SE.PauseEvent, if isPaused then "unpause" else "pause")
      , may isPaused (NoHighlight, keyM SE.RunSingleTickEvent, "step")
      , may
          (isPaused && hasDebug)
          ( if uig ^. uiShowDebug then Alert else NoHighlight
          , keyM SE.ShowCESKDebugEvent
          , "debug"
          )
      , Just (NoHighlight, keyM SE.IncreaseTpsEvent <> "/" <> keyM SE.DecreaseTpsEvent, "speed")
      , Just
          ( NoHighlight
          , keyM SE.ToggleREPLVisibilityEvent
          , if uig ^. uiShowREPL then "hide REPL" else "show REPL"
          )
      , Just
          ( if uig ^. uiShowRobots then NoHighlight else Alert
          , keyM SE.HideRobotsEvent
          , "hide robots"
          )
      ]
  may b = if b then Just else const Nothing

  highlightKeyCmds (k, n) = (PanelSpecific, k, n)

  keyCmdsFor (Just (FocusablePanel WorldEditorPanel)) =
    [("^s", "save map")]
  keyCmdsFor (Just (FocusablePanel REPLPanel)) =
    [ ("↓↑", "history")
    ]
      ++ [("Enter", "execute") | not isReplWorking]
      ++ [(keyR SE.CancelRunningProgramEvent, "cancel") | isReplWorking]
      ++ [(keyR SE.TogglePilotingModeEvent, renderPilotModeSwitch ctrlMode) | creative]
      ++ [(keyR SE.ToggleCustomKeyHandlingEvent, renderHandlerModeSwitch ctrlMode) | handlerInstalled]
      ++ [("PgUp/Dn", "scroll")]
  keyCmdsFor (Just (FocusablePanel WorldPanel)) =
    [(T.intercalate "/" $ map keyW enumerate, "scroll") | canScroll]
      ++ [(keyW SE.ViewBaseEvent, "recenter") | not viewingBase]
      ++ [(keyW SE.ShowFpsEvent, "FPS")]
  keyCmdsFor (Just (FocusablePanel RobotPanel)) =
    ("Enter", "pop out")
      : if isJust inventorySearch
        then [("Esc", "exit search")]
        else
          [ (keyE SE.MakeEntityEvent, "make")
          , (keyE SE.ShowZeroInventoryEntitiesEvent, (if showZero then "hide" else "show") <> " 0")
          ,
            ( keyE SE.SwitchInventorySortDirection <> "/" <> keyE SE.CycleInventorySortEvent
            , T.unwords ["Sort:", renderSortMethod inventorySort]
            )
          , (keyE SE.SearchInventoryEvent, "search")
          ]
  keyCmdsFor (Just (FocusablePanel InfoPanel)) = []
  keyCmdsFor _ = []
  keyM = VU.bindingText s . SE.Main
  keyR = VU.bindingText s . SE.REPL
  keyE = VU.bindingText s . SE.Robot
  keyW = VU.bindingText s . SE.World

data KeyHighlight = NoHighlight | Alert | PanelSpecific

-- | Draw a single key command in the menu.
drawKeyCmd :: (KeyHighlight, Text, Text) -> Widget Name
drawKeyCmd (h, key, cmd) =
  hBox
    [ withAttr attr (txt $ brackets key)
    , txt cmd
    ]
 where
  attr = case h of
    NoHighlight -> defAttr
    Alert -> notifAttr
    PanelSpecific -> highlightAttr

------------------------------------------------------------
-- World panel
------------------------------------------------------------

-- | Compare to: 'Swarm.Util.Content.getMapRectangle'
worldWidget ::
  (Cosmic Coords -> Widget n) ->
  -- | view center
  Cosmic Location ->
  Widget n
worldWidget renderCoord gameViewCenter = Widget Fixed Fixed $
  do
    ctx <- getContext
    let w = ctx ^. availWidthL
        h = ctx ^. availHeightL
        vr = viewingRegion gameViewCenter (fromIntegral w, fromIntegral h)
        ixs = range $ vr ^. planar
    render . vBox . map hBox . chunksOf w . map (renderCoord . Cosmic (vr ^. subworld)) $ ixs

-- | Draw the current world view.
drawWorldPane :: UIGameplay -> GameState -> Widget Name
drawWorldPane ui g =
  center
    . cached WorldCache
    . reportExtent WorldExtent
    -- Set the clickable request after the extent to play nice with the cache
    . clickable (FocusablePanel WorldPanel)
    $ worldWidget renderCoord (g ^. robotInfo . viewCenter)
 where
  renderCoord = drawLoc ui g

------------------------------------------------------------
-- Robot inventory panel
------------------------------------------------------------

-- | Draw info about the currently focused robot, such as its name,
--   position, orientation, and inventory, as long as it is not too
--   far away.
drawRobotPanel :: AppState -> Widget Name
drawRobotPanel s
  -- If the focused robot is too far away to communicate, just leave the panel blank.
  -- There should be no way to tell the difference between a robot that is too far
  -- away and a robot that does not exist.
  | Just r <- s ^. gameState . to focusedRobot
  , Just (_, lst) <- s ^. uiState . uiGameplay . uiInventory . uiInventoryList =
      let aMap = maybe mempty (view (scenarioLandscape . scenarioCosmetics) . fst) $ s ^. uiState . uiGameplay . scenarioRef
          drawClickableItem pos selb = clickable (InventoryListItem pos) . drawItem aMap (lst ^. BL.listSelectedL) pos selb
          details =
            [ txt (r ^. robotName)
            , padLeft (Pad 2) . str . renderCoordsString $ r ^. robotLocation
            , padLeft (Pad 2) $ renderDisplay aMap (r ^. robotDisplay)
            ]
       in padBottom Max $
            vBox
              [ hCenter $ hBox details
              , withLeftPaddedVScrollBars . padLeft (Pad 1) . padTop (Pad 1) $
                  BL.renderListWithIndex drawClickableItem True lst
              ]
  | otherwise = blank

blank :: Widget Name
blank = padRight Max . padBottom Max $ str " "

-- | Draw an inventory entry.
drawItem ::
  M.Map WorldAttr PreservableColor ->
  -- | The index of the currently selected inventory entry
  Maybe Int ->
  -- | The index of the entry we are drawing
  Int ->
  -- | Whether this entry is selected; we can ignore this
  --   because it will automatically have a special attribute
  --   applied to it.
  Bool ->
  -- | The entry to draw.
  InventoryListEntry ->
  Widget Name
drawItem _ sel i _ (Separator l) =
  -- Make sure a separator right before the focused element is
  -- visible. Otherwise, when a separator occurs as the very first
  -- element of the list, once it scrolls off the top of the viewport
  -- it will never become visible again.
  -- See https://github.com/jtdaugherty/brick/issues/336#issuecomment-921220025
  applyWhen (sel == Just (i + 1)) visible $ hBorderWithLabel (txt l)
drawItem aMap _ _ _ (InventoryEntry n e) = drawLabelledEntityName aMap e <+> showCount n
 where
  showCount = padLeft Max . str . show
drawItem aMap _ _ _ (EquippedEntry e) = drawLabelledEntityName aMap e <+> padLeft Max (str " ")

------------------------------------------------------------
-- Info panel
------------------------------------------------------------

-- | Draw the info panel in the bottom-left corner, which shows info
--   about the currently focused inventory item.
drawInfoPanel :: AppState -> Widget Name
drawInfoPanel s
  | Just Far <- s ^. gameState . to focusedRange = blank
  | otherwise =
      withVScrollBars OnRight
        . viewport InfoViewport Vertical
        . padLeftRight 1
        $ explainFocusedItem s

-- | Display info about the currently focused inventory entity,
--   such as its description and relevant recipes.
explainFocusedItem :: AppState -> Widget Name
explainFocusedItem s = case focusedItem s of
  Just (InventoryEntry _ e) -> explainEntry uig gs e
  Just (EquippedEntry e) -> explainEntry uig gs e
  _ -> txt " "
 where
  gs = s ^. gameState
  uig = s ^. uiState . uiGameplay

explainEntry :: UIGameplay -> GameState -> Entity -> Widget Name
explainEntry uig gs e =
  vBox $
    [ displayProperties $ Set.toList (e ^. entityProperties)
    , drawMarkdown (e ^. entityDescription)
    , explainCapabilities gs e
    , explainRecipes gs e
    ]
      <> [drawRobotMachine gs False | CDebug `M.member` getMap (e ^. entityCapabilities)]
      <> [drawRobotLog uig gs | CExecute Log `M.member` getMap (e ^. entityCapabilities)]

displayProperties :: [EntityProperty] -> Widget Name
displayProperties = displayList . mapMaybe showProperty
 where
  showProperty Growable = Just "growing"
  showProperty Pushable = Just "pushable"
  showProperty Combustible = Just "combustible"
  showProperty Infinite = Just "infinite"
  showProperty Liquid = Just "liquid"
  showProperty Unwalkable = Just "blocking"
  showProperty Opaque = Just "opaque"
  showProperty Boundary = Just "boundary"
  -- Most things are pickable so we don't show that.
  showProperty Pickable = Nothing
  -- 'Known' is just a technical detail of how we handle some entities
  -- in challenge scenarios and not really something the player needs
  -- to know.
  showProperty Known = Nothing
  showProperty Printable = Just "printable"

  displayList [] = emptyWidget
  displayList ps =
    vBox
      [ hBox . L.intersperse (txt ", ") . map (withAttr robotAttr . txt) $ ps
      , txt " "
      ]

-- | This widget can have potentially multiple "headings"
-- (one per capability), each with multiple commands underneath.
-- Directly below each heading there will be a "exercise cost"
-- description, unless the capability is free-to-exercise.
explainCapabilities :: GameState -> Entity -> Widget Name
explainCapabilities gs e
  | null capabilitiesAndCommands = emptyWidget
  | otherwise =
      padBottom (Pad 1) $
        vBox
          [ hBorderWithLabel (txt "Enabled commands")
          , hCenter
              . vBox
              . L.intersperse (txt " ") -- Inserts an extra blank line between major "Cost" sections
              $ map drawSingleCapabilityWidget capabilitiesAndCommands
          ]
 where
  eLookup = lookupEntityE $ entitiesByName $ gs ^. landscape . terrainAndEntities . entityMap
  eitherCosts = (traverse . traverse) eLookup $ e ^. entityCapabilities
  capabilitiesAndCommands = case eitherCosts of
    Right eCaps -> M.elems . getMap . commandsForDeviceCaps $ eCaps
    Left x ->
      error $
        unwords
          [ "Error: somehow an invalid entity reference escaped the parse-time check"
          , T.unpack x
          ]

  drawSingleCapabilityWidget cmdsAndCost =
    vBox
      [ costWidget cmdsAndCost
      , padLeft (Pad 1) . vBox . map renderCmdInfo . NE.toList $ enabledCommands cmdsAndCost
      ]

  renderCmdInfo c =
    Widget Fixed Fixed $ do
      ctx <- getContext
      let w = ctx ^. availWidthL
          constType = inferConst c
          info = constInfo c
          requiredWidthForTypes = textWidth (syntax info <> " : " <> prettyTextLine constType)
      render
        . padTop (Pad 1)
        $ vBox
          [ hBox
              [ padRight (Pad 1) (txt $ syntax info)
              , padRight (Pad 1) (txt ":")
              , if requiredWidthForTypes <= w
                  then withAttr magentaAttr . txt $ prettyTextLine constType
                  else emptyWidget
              ]
          , hBox $
              if requiredWidthForTypes > w
                then
                  [ padRight (Pad 1) (txt " ")
                  , withAttr magentaAttr . txt $ prettyTextWidth constType (w - 2)
                  ]
                else [emptyWidget]
          , padTop (Pad 1) . padLeft (Pad 1) . txtWrap . briefDoc $ constDoc info
          ]

  costWidget cmdsAndCost =
    if null ings
      then emptyWidget
      else padTop (Pad 1) $ vBox $ withAttr boldAttr (txt "Cost:") : map drawCost ings
   where
    ings = ingredients $ commandCost cmdsAndCost

  drawCost (n, ingr) =
    padRight (Pad 1) (str (show n)) <+> eName
   where
    eName = applyEntityNameAttr Nothing missing ingr $ txt $ ingr ^. entityName
    missing = E.lookup ingr robotInv < n

  robotInv = fromMaybe E.empty $ gs ^? to focusedRobot . _Just . robotInventory

explainRecipes :: GameState -> Entity -> Widget Name
explainRecipes gs e
  | null recipes = emptyWidget
  | otherwise =
      vBox
        [ padBottom (Pad 1) (hBorderWithLabel (txt "Recipes"))
        , padLeftRight 2
            . hCenter
            . vBox
            $ map (hLimit widthLimit . padBottom (Pad 1) . drawRecipe (Just e) inv) recipes
        ]
 where
  recipes = recipesWith gs e

  inv = fromMaybe E.empty $ gs ^? to focusedRobot . _Just . robotInventory

  width (n, ingr) =
    length (show n) + 1 + maximum0 (map T.length . T.words $ ingr ^. entityName)

  maxInputWidth =
    fromMaybe 0 $
      maximumOf (traverse . recipeInputs . traverse . to width) recipes
  maxOutputWidth =
    fromMaybe 0 $
      maximumOf (traverse . recipeOutputs . traverse . to width) recipes
  widthLimit = 2 * max maxInputWidth maxOutputWidth + 11

-- | Return all recipes that involve a given entity.
recipesWith :: GameState -> Entity -> [Recipe Entity]
recipesWith gs e =
  let getRecipes select = recipesFor (gs ^. recipesInfo . select) e
   in -- The order here is chosen intentionally.  See https://github.com/swarm-game/swarm/issues/418.
      --
      --   1. Recipes where the entity is an input --- these should go
      --     first since the first thing you will want to know when you
      --     obtain a new entity is what you can do with it.
      --
      --   2. Recipes where it serves as a catalyst --- for the same reason.
      --
      --   3. Recipes where it is an output --- these should go last,
      --      since if you have it, you probably already figured out how
      --      to make it.
      L.nub $
        concat
          [ getRecipes recipesIn
          , getRecipes recipesCat
          , getRecipes recipesOut
          ]

-- | Draw an ASCII art representation of a recipe.  For now, the
--   weight is not shown.
drawRecipe :: Maybe Entity -> Inventory -> Recipe Entity -> Widget Name
drawRecipe me inv (Recipe ins outs reqs time _weight) =
  vBox
    -- any requirements (e.g. furnace) go on top.
    [ hCenter $ drawReqs reqs
    , -- then we draw inputs, a connector, and outputs.
      hBox
        [ vBox (zipWith drawIn [0 ..] (ins <> times))
        , connector
        , vBox (zipWith drawOut [0 ..] outs)
        ]
    ]
 where
  -- The connector is either just a horizontal line ─────
  -- or, if there are requirements, a horizontal line with
  -- a vertical piece coming out of the center, ──┴── .
  connector
    | null reqs = hLimit 5 hBorder
    | otherwise =
        hBox
          [ hLimit 2 hBorder
          , joinableBorder (Edges True False True True)
          , hLimit 2 hBorder
          ]
  inLen = length ins + length times
  outLen = length outs
  times = [(fromIntegral time, timeE) | time /= 1]

  -- Draw inputs and outputs.
  drawIn, drawOut :: Int -> (Count, Entity) -> Widget Name
  drawIn i (n, ingr) =
    hBox
      [ padRight (Pad 1) $ str (show n) -- how many?
      , fmtEntityName missing ingr -- name of the input
      , padLeft (Pad 1) $ -- a connecting line:   ─────┬
          hBorder
            <+> ( joinableBorder (Edges (i /= 0) (i /= inLen - 1) True False) -- ...maybe plus vert ext:   │
                    <=> if i /= inLen - 1
                      then vLimit (subtract 1 . length . T.words $ ingr ^. entityName) vBorder
                      else emptyWidget
                )
      ]
   where
    missing = E.lookup ingr inv < n

  drawOut i (n, ingr) =
    hBox
      [ padRight (Pad 1) $
          ( joinableBorder (Edges (i /= 0) (i /= outLen - 1) False True)
              <=> if i /= outLen - 1
                then vLimit (subtract 1 . length . T.words $ ingr ^. entityName) vBorder
                else emptyWidget
          )
            <+> hBorder
      , fmtEntityName False ingr
      , padLeft (Pad 1) $ str (show n)
      ]

  -- If it's the focused entity, draw it highlighted.
  -- If the robot doesn't have any, draw it in red.
  fmtEntityName :: Bool -> Entity -> Widget n
  fmtEntityName missing ingr =
    applyEntityNameAttr me missing ingr $ txtLines nm
   where
    -- Split up multi-word names, one line per word
    nm = ingr ^. entityName
    txtLines = vBox . map txt . T.words

applyEntityNameAttr :: Maybe Entity -> Bool -> Entity -> (Widget n -> Widget n)
applyEntityNameAttr me missing ingr
  | Just ingr == me = withAttr highlightAttr
  | ingr == timeE = withAttr yellowAttr
  | missing = withAttr invalidFormInputAttr
  | otherwise = id

-- | Ad-hoc entity to represent time - only used in recipe drawing
timeE :: Entity
timeE = mkEntity (defaultEntityDisplay '.') "ticks" mempty [] mempty

drawReqs :: IngredientList Entity -> Widget Name
drawReqs = vBox . map (hCenter . drawReq)
 where
  drawReq (1, e) = txt $ e ^. entityName
  drawReq (n, e) = str (show n) <+> txt " " <+> txt (e ^. entityName)

indent2 :: WrapSettings
indent2 = defaultWrapSettings {fillStrategy = FillIndent 2}

-- | Only show the most recent entry, and any entries which were
--   produced by "say" or "log" commands.  Other entries (i.e. errors
--   or command status reports) are thus ephemeral, i.e. they are only
--   shown when they are the most recent log entry, but hidden once
--   something else is logged.
getLogEntriesToShow :: GameState -> [LogEntry]
getLogEntriesToShow gs = logEntries ^.. traversed . ifiltered shouldShow
 where
  logEntries = gs ^. to focusedRobot . _Just . robotLog
  n = Seq.length logEntries

  shouldShow i le =
    (i == n - 1) || case le ^. leSource of
      RobotLog src _ _ -> src `elem` [Said, Logged]
      SystemLog -> False

drawRobotLog :: UIGameplay -> GameState -> Widget Name
drawRobotLog uig gs =
  vBox
    [ padBottom (Pad 1) (hBorderWithLabel (txt "Log"))
    , vBox . F.toList . imap drawEntry $ logEntriesToShow
    ]
 where
  logEntriesToShow = getLogEntriesToShow gs
  n = length logEntriesToShow
  drawEntry i e =
    applyWhen (i == n - 1 && uig ^. uiScrollToEnd) visible $
      drawLogEntry (not allMe) e

  rid = gs ^? to focusedRobot . _Just . robotID

  allMe = all me logEntriesToShow
  me le = case le ^. leSource of
    RobotLog _ i _ -> Just i == rid
    _ -> False

-- | Show the 'CESK' machine of focused robot. Puts a separator above.
drawRobotMachine :: GameState -> Bool -> Widget Name
drawRobotMachine gs showName = case gs ^. to focusedRobot of
  Nothing -> machineLine "no selected robot"
  Just r ->
    vBox
      [ machineLine $ r ^. robotName <> "#" <> r ^. robotID . to tshow
      , txt $ r ^. machine . to prettyText
      ]
 where
  tshow = T.pack . show
  hLine t = padBottom (Pad 1) (hBorderWithLabel (txt t))
  machineLine r = hLine $ if showName then "Machine [" <> r <> "]" else "Machine"

-- | Draw one log entry with an optional robot name first.
drawLogEntry :: Bool -> LogEntry -> Widget a
drawLogEntry addName e =
  withAttr (colorLogs e) . txtWrapWith indent2 $
    if addName then name else t
 where
  t = e ^. leText
  name =
    "["
      <> view leName e
      <> "] "
      <> case e ^. leSource of
        RobotLog Said _ _ -> "said " <> quote t
        _ -> t

------------------------------------------------------------
-- REPL panel
------------------------------------------------------------

-- | Turn the repl prompt into a decorator for the form
replPromptAsWidget :: Text -> REPLPrompt -> Widget Name
replPromptAsWidget _ (CmdPrompt _) = txt "> "
replPromptAsWidget t (SearchPrompt rh) =
  case lastEntry t rh of
    Nothing -> txt "[nothing found] "
    Just lastentry
      | T.null t -> txt "[find] "
      | otherwise -> txt $ "[found: \"" <> lastentry <> "\"] "

renderREPLPrompt :: FocusRing Name -> REPLState -> Widget Name
renderREPLPrompt focus theRepl = ps1 <+> replE
 where
  prompt = theRepl ^. replPromptType
  replEditor = theRepl ^. replPromptEditor
  color t =
    case theRepl ^. replValid of
      Right () -> txt t
      Left NoLoc -> withAttr redAttr (txt t)
      Left (SrcLoc s e) | s == e || s >= T.length t -> withAttr redAttr (txt t)
      Left (SrcLoc s e) ->
        let (validL, (invalid, validR)) = T.splitAt (e - s) <$> T.splitAt s t
         in hBox [txt validL, withAttr redAttr (txt invalid), txt validR]
  ps1 = replPromptAsWidget (T.concat $ getEditContents replEditor) prompt
  replE =
    renderEditor
      (vBox . map color)
      (focusGetCurrent focus `elem` [Nothing, Just (FocusablePanel REPLPanel), Just REPLInput])
      replEditor

-- | Draw the REPL.
drawREPL :: AppState -> Widget Name
drawREPL s =
  vBox
    [ withLeftPaddedVScrollBars
        . viewport REPLViewport Vertical
        . vBox
        $ [cached REPLHistoryCache (vBox history), currentPrompt]
    , vBox mayDebug
    ]
 where
  uig = s ^. uiState . uiGameplay
  gs = s ^. gameState

  -- rendered history lines fitting above REPL prompt
  history :: [Widget n]
  history = map fmt . filter (not . isREPLSaved) . toList . getSessionREPLHistoryItems $ theRepl ^. replHistory
  currentPrompt :: Widget Name
  currentPrompt = case (isActive <$> base, theRepl ^. replControlMode) of
    (_, Handling) -> padRight Max $ txt "[key handler running, M-k to toggle]"
    (Just False, _) -> renderREPLPrompt (uig ^. uiFocusRing) theRepl
    _running -> padRight Max $ txt "..."
  theRepl = uig ^. uiREPL

  -- NOTE: there exists a lens named 'baseRobot' that uses "unsafe"
  -- indexing that may be an alternative to this:
  base = gs ^. robotInfo . robotMap . at 0

  fmt (REPLHistItem itemType t) = case itemType of
    REPLEntry {} -> txt $ "> " <> t
    REPLOutput -> txt t
    REPLError -> txtWrapWith indent2 {preserveIndentation = True} t
  mayDebug = [drawRobotMachine gs True | uig ^. uiShowDebug]

------------------------------------------------------------
-- Utility
------------------------------------------------------------

-- See https://github.com/jtdaugherty/brick/discussions/484
withLeftPaddedVScrollBars :: Widget n -> Widget n
withLeftPaddedVScrollBars =
  withVScrollBarRenderer (addLeftSpacing verticalScrollbarRenderer)
    . withVScrollBars OnRight
 where
  addLeftSpacing :: VScrollbarRenderer n -> VScrollbarRenderer n
  addLeftSpacing r =
    r
      { scrollbarWidthAllocation = 2
      , renderVScrollbar = hLimit 1 $ renderVScrollbar r
      , renderVScrollbarTrough = hLimit 1 $ renderVScrollbarTrough r
      }
