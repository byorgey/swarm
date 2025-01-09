{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Application state for the @brick@-based Swarm TUI.
module Swarm.TUI.Model (
  -- * Custom UI label types
  -- $uilabel
  AppEvent (..),
  FocusablePanel (..),
  Name (..), -- helps to minimize import lines

  -- ** Web command
  WebCommand (..),
  WebInvocationState (..),
  RejectionReason (..),

  -- * Menus and dialogs
  ModalType (..),
  ScenarioOutcome (..),
  Button (..),
  ButtonAction (..),
  Modal (..),
  modalType,
  modalDialog,
  MainMenuEntry (..),
  mainMenu,
  Menu (..),
  _NewGameMenu,
  mkScenarioList,
  mkNewGameMenu,

  -- * UI state

  -- ** Inventory
  InventoryListEntry (..),
  _Separator,
  _InventoryEntry,
  _EquippedEntry,

  -- ** Updating
  populateInventoryList,
  infoScroll,
  modalScroll,
  replScroll,

  -- ** Utility
  logEvent,
  SwarmKeyDispatcher,
  KeyEventHandlingState (KeyEventHandlingState),
  SwarmKeyDispatchers (..),
  keyConfig,
  keyDispatchers,

  -- * App state
  AppState (AppState),
  gameState,
  uiState,
  keyEventHandling,
  runtimeState,

  -- ** Initialization
  AppOpts (..),
  defaultAppOpts,

  -- *** Re-exported types used in options
  ColorMode (..),

  -- ** Utility
  focusedItem,
  focusedEntity,
  nextScenario,
) where

import Brick (EventM, ViewportScroll, viewportScroll)
import Brick.Keybindings as BK
import Brick.Widgets.List qualified as BL
import Control.Lens hiding (from, (<.>))
import Control.Monad ((>=>))
import Control.Monad.State (MonadState)
import Data.List (findIndex)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector qualified as V
import GitHash (GitInfo)
import Graphics.Vty (ColorMode (..))
import Network.Wai.Handler.Warp (Port)
import Swarm.Game.Entity as E
import Swarm.Game.Ingredients
import Swarm.Game.Robot
import Swarm.Game.Scenario.Status
import Swarm.Game.ScenarioInfo (_SISingle)
import Swarm.Game.State
import Swarm.Game.State.Runtime
import Swarm.Game.State.Substate
import Swarm.Game.Tick (TickNumber (..))
import Swarm.Game.World.Gen (Seed)
import Swarm.Log
import Swarm.TUI.Inventory.Sorting
import Swarm.TUI.Model.DebugOption (DebugOption)
import Swarm.TUI.Model.Event (SwarmEvent)
import Swarm.TUI.Model.Menu
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.UI
import Swarm.TUI.Model.UI.Gameplay
import Swarm.TUI.Model.WebCommand (RejectionReason (..), WebCommand (..), WebInvocationState (..))
import Swarm.Util.Lens (makeLensesNoSigs)
import Text.Fuzzy qualified as Fuzzy

------------------------------------------------------------
-- Custom UI label types
------------------------------------------------------------

-- $uilabel These types are used as parameters to various @brick@
-- types.

-- | 'Swarm.TUI.Model.AppEvent' represents a type for custom event types our app can
--   receive. The primary custom event 'Frame' is sent by a separate thread as fast as
--   it can, telling the TUI to render a new frame.
data AppEvent
  = Frame
  | Web WebCommand
  | UpstreamVersion (Either (Severity, Text) String)

infoScroll :: ViewportScroll Name
infoScroll = viewportScroll InfoViewport

modalScroll :: ViewportScroll Name
modalScroll = viewportScroll ModalViewport

replScroll :: ViewportScroll Name
replScroll = viewportScroll REPLViewport

--------------------------------------------------
-- Utility

-- | Simply log to the runtime event log.
logEvent :: LogSource -> Severity -> Text -> Text -> Notifications LogEntry -> Notifications LogEntry
logEvent src sev who msg el =
  el
    & notificationsCount %~ succ
    & notificationsContent %~ (l :)
 where
  l = LogEntry (TickNumber 0) src sev who msg

data KeyEventHandlingState = KeyEventHandlingState
  { _keyConfig :: KeyConfig SwarmEvent
  , _keyDispatchers :: SwarmKeyDispatchers
  }

type SwarmKeyDispatcher = KeyDispatcher SwarmEvent (EventM Name AppState)

data SwarmKeyDispatchers = SwarmKeyDispatchers
  { mainGameDispatcher :: SwarmKeyDispatcher
  , replDispatcher :: SwarmKeyDispatcher
  , worldDispatcher :: SwarmKeyDispatcher
  , robotDispatcher :: SwarmKeyDispatcher
  }

-- | This encapsulates both game and UI state for an actively-playing scenario.
data PlayState = PlayState
  { _gameState :: GameState
  , _somethingState :: ()
  }

--------------------------------------------------
-- Lenses for PlayState

makeLensesNoSigs ''PlayState

-- | The 'GameState' record.
gameState :: Lens' PlayState GameState

-- | The 'UIGameplay' record.
somethingState :: Lens' PlayState ()

-- ----------------------------------------------------------------------------
--                                   APPSTATE                                --
-- ----------------------------------------------------------------------------

-- | The 'AppState' just stores together the other states.
--
-- This is so you can use a smaller state when e.g. writing some game logic
-- or updating the UI. Also consider that GameState can change when loading
-- a new scenario - if the state should persist games, use RuntimeState.
data AppState = AppState
  { _playState :: PlayState
  , _uiState :: UIState
  , _keyEventHandling :: KeyEventHandlingState
  , _runtimeState :: RuntimeState
  }

------------------------------------------------------------
-- Functions for updating the UI state
------------------------------------------------------------

-- | Given the focused robot, populate the UI inventory list in the info
--   panel with information about its inventory.
populateInventoryList :: (MonadState UIInventory m) => Maybe Robot -> m ()
populateInventoryList Nothing = uiInventoryList .= Nothing
populateInventoryList (Just r) = do
  mList <- preuse $ uiInventoryList . _Just . _2
  showZero <- use uiShowZero
  sortOptions <- use uiInventorySort
  search <- use uiInventorySearch
  let mkInvEntry (n, e) = InventoryEntry n e
      mkInstEntry (_, e) = EquippedEntry e
      itemList isInventoryDisplay mk label =
        (\case [] -> []; xs -> Separator label : xs)
          . map mk
          . sortInventory sortOptions
          . filter ((&&) <$> matchesSearch <*> shouldDisplay)
          . elems
       where
        -- Display items if we have a positive number of them, or they
        -- aren't an equipped device.  In other words we don't need to
        -- display equipped devices twice unless we actually have some
        -- in our inventory in addition to being equipped.
        shouldDisplay (n, e) =
          n > 0
            || isInventoryDisplay
              && showZero
              && not ((r ^. equippedDevices) `E.contains` e)

      matchesSearch :: (Count, Entity) -> Bool
      matchesSearch (_, e) = maybe (const True) Fuzzy.test search (e ^. E.entityName)

      items =
        (r ^. robotInventory . to (itemList True mkInvEntry "Compendium"))
          ++ (r ^. equippedDevices . to (itemList False mkInstEntry "Equipped devices"))

      -- Attempt to keep the selected element steady.
      sel = mList >>= BL.listSelectedElement -- Get the currently selected element+index.
      idx = case sel of
        -- If there is no currently selected element, just focus on
        -- index 1 (not 0, to avoid the separator).
        Nothing -> 1
        -- Otherwise, try to find the same entry in the list;
        -- if it's not there, keep the index the same.
        Just (selIdx, InventoryEntry _ e) ->
          fromMaybe selIdx (findIndex ((== Just e) . preview (_InventoryEntry . _2)) items)
        Just (selIdx, EquippedEntry e) ->
          fromMaybe selIdx (findIndex ((== Just e) . preview _EquippedEntry) items)
        Just (selIdx, _) -> selIdx

      -- Create the new list, focused at the desired index.
      lst = BL.listMoveTo idx $ BL.list InventoryList (V.fromList items) 1

  -- Finally, populate the newly created list in the UI, and remember
  -- the hash of the current robot.
  uiInventoryList .= Just (r ^. inventoryHash, lst)

------------------------------------------------------------
-- App state (= UI state + game state) initialization
------------------------------------------------------------

-- | Command-line options for configuring the app.
data AppOpts = AppOpts
  { userSeed :: Maybe Seed
  -- ^ Explicit seed chosen by the user.
  , userScenario :: Maybe FilePath
  -- ^ Scenario the user wants to play.
  , scriptToRun :: Maybe FilePath
  -- ^ Code to be run on base.
  , pausedAtStart :: Bool
  -- ^ Pause the game on start by default.
  , autoPlay :: Bool
  -- ^ Automatically run the solution defined in the scenario file
  , autoShowObjectives :: Bool
  -- ^ Show objectives dialogs when an objective is achieved/failed.
  , speed :: Int
  -- ^ Initial game speed (logarithm)
  , debugOptions :: Set DebugOption
  -- ^ Debugging options, for example show creative switch.
  , colorMode :: Maybe ColorMode
  -- ^ What colour mode should be used?
  , userWebPort :: Maybe Port
  -- ^ Explicit port on which to run the web API
  , repoGitInfo :: Maybe GitInfo
  -- ^ Information about the Git repository (not present in release).
  }

-- | A default/empty 'AppOpts' record.
defaultAppOpts :: AppOpts
defaultAppOpts =
  AppOpts
    { userSeed = Nothing
    , userScenario = Nothing
    , scriptToRun = Nothing
    , pausedAtStart = False
    , autoShowObjectives = True
    , autoPlay = False
    , speed = defaultInitLgTicksPerSecond
    , debugOptions = mempty
    , colorMode = Nothing
    , userWebPort = Nothing
    , repoGitInfo = Nothing
    }

-- | Extract the scenario which would come next in the menu from the
--   currently selected scenario (if any).  Can return @Nothing@ if
--   either we are not in the @NewGameMenu@, or the current scenario
--   is the last among its siblings.
nextScenario :: Menu -> Maybe ScenarioInfoPair
nextScenario = \case
  NewGameMenu (curMenu :| _) ->
    let nextMenuList = BL.listMoveDown curMenu
        isLastScenario = BL.listSelected curMenu == Just (length (BL.listElements curMenu) - 1)
     in if isLastScenario
          then Nothing
          else BL.listSelectedElement nextMenuList >>= preview _SISingle . snd
  _ -> Nothing

--------------------------------------------------
-- Lenses for KeyEventHandlingState

makeLensesNoSigs ''KeyEventHandlingState

-- | Keybindings (possibly customized by player) for 'SwarmEvent's.
keyConfig :: Lens' KeyEventHandlingState (KeyConfig SwarmEvent)

-- | Dispatchers that will call handler on key combo.
keyDispatchers :: Lens' KeyEventHandlingState SwarmKeyDispatchers
--------------------------------------------------
-- Lenses for AppState

playState :: Functor f => (PlayState -> f PlayState) -> AppState -> f AppState

makeLensesNoSigs ''AppState

-- | The 'UIState' record.
uiState :: Lens' AppState UIState

-- | The key event handling configuration.
keyEventHandling :: Lens' AppState KeyEventHandlingState

-- | The 'RuntimeState' record
runtimeState :: Lens' AppState RuntimeState

--------------------------------------------------
-- Utility functions

-- | Get the currently focused 'InventoryListEntry' from the robot
--   info panel (if any).
focusedItem :: AppState -> Maybe InventoryListEntry
focusedItem s = do
  list <- s ^? uiState . uiGameplay . uiInventory . uiInventoryList . _Just . _2
  (_, entry) <- BL.listSelectedElement list
  return entry

-- | Get the currently focused entity from the robot info panel (if
--   any).  This is just like 'focusedItem' but forgets the
--   distinction between plain inventory items and equipped devices.
focusedEntity :: AppState -> Maybe Entity
focusedEntity =
  focusedItem >=> \case
    Separator _ -> Nothing
    InventoryEntry _ e -> Just e
    EquippedEntry e -> Just e
