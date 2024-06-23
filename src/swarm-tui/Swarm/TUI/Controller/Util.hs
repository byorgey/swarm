{-# LANGUAGE PatternSynonyms #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Controller.Util where

import Brick hiding (Direction)
import Brick.Focus
import Control.Carrier.Lift qualified as Fused
import Control.Carrier.State.Lazy qualified as Fused
import Control.Lens
import Control.Lens qualified as Lens
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Extra (whenJust)
import Control.Monad.IO.Class (MonadIO (liftIO), liftIO)
import Control.Monad.State (MonadState, execState)
import Data.Map qualified as M
import Data.Set qualified as S
import Graphics.Vty qualified as V
import Swarm.Effect (TimeIOC, runTimeIO)
import Swarm.Game.Device
import Swarm.Game.Robot (robotCapabilities)
import Swarm.Game.State
import Swarm.Game.State.Landscape
import Swarm.Game.State.Robot
import Swarm.Game.State.Substate
import Swarm.Game.Step (finishGameTick)
import Swarm.Game.Universe
import Swarm.Game.World qualified as W
import Swarm.Game.World.Coords
import Swarm.Language.Capability (Capability (CDebug))
import Swarm.TUI.Model
import Swarm.TUI.Model.UI
import Swarm.TUI.View.Util (generateModal)
import System.Clock (Clock (..), getTime)

-- | Pattern synonyms to simplify brick event handler
pattern Key :: V.Key -> BrickEvent n e
pattern Key k = VtyEvent (V.EvKey k [])

pattern CharKey, ControlChar, MetaChar :: Char -> BrickEvent n e
pattern CharKey c = VtyEvent (V.EvKey (V.KChar c) [])
pattern ControlChar c = VtyEvent (V.EvKey (V.KChar c) [V.MCtrl])
pattern MetaChar c = VtyEvent (V.EvKey (V.KChar c) [V.MMeta])

pattern ShiftKey :: V.Key -> BrickEvent n e
pattern ShiftKey k = VtyEvent (V.EvKey k [V.MShift])

pattern MetaKey :: V.Key -> BrickEvent n e
pattern MetaKey k = VtyEvent (V.EvKey k [V.MMeta])

pattern EscapeKey :: BrickEvent n e
pattern EscapeKey = VtyEvent (V.EvKey V.KEsc [])

pattern BackspaceKey :: BrickEvent n e
pattern BackspaceKey = VtyEvent (V.EvKey V.KBS [])

pattern FKey :: Int -> BrickEvent n e
pattern FKey c = VtyEvent (V.EvKey (V.KFun c) [])

openModal :: ModalType -> EventM Name AppState ()
openModal mt = do
  resetViewport modalScroll
  newModal <- gets $ flip generateModal mt
  ensurePause
  uiState . uiGameplay . uiModal ?= newModal
  -- Beep
  case mt of
    ScenarioEndModal _ -> do
      vty <- getVtyHandle
      liftIO $ V.ringTerminalBell $ V.outputIface vty
    _ -> return ()
 where
  -- Set the game to AutoPause if needed
  ensurePause = do
    pause <- use $ gameState . temporal . paused
    unless (pause || isRunningModal mt) $ do
      gameState . temporal . runStatus .= AutoPause

-- | The running modals do not autopause the game.
isRunningModal :: ModalType -> Bool
isRunningModal = \case
  RobotsModal -> True
  MessagesModal -> True
  _ -> False

-- | Set the game to Running if it was (auto) paused otherwise to paused.
--
-- Also resets the last frame time to now. If we are pausing, it
-- doesn't matter; if we are unpausing, this is critical to
-- ensure the next frame doesn't think it has to catch up from
-- whenever the game was paused!
safeTogglePause :: EventM Name AppState ()
safeTogglePause = do
  curTime <- liftIO $ getTime Monotonic
  uiState . uiGameplay . uiTiming . lastFrameTime .= curTime
  uiState . uiGameplay . uiShowDebug .= False
  p <- gameState . temporal . runStatus Lens.<%= toggleRunStatus
  when (p == Running) $ zoomGameState finishGameTick

-- | Only unpause the game if leaving autopaused modal.
--
-- Note that the game could have been paused before opening
-- the modal, in that case, leave the game paused.
safeAutoUnpause :: EventM Name AppState ()
safeAutoUnpause = do
  runs <- use $ gameState . temporal . runStatus
  when (runs == AutoPause) safeTogglePause

toggleModal :: ModalType -> EventM Name AppState ()
toggleModal mt = do
  modal <- use $ uiState . uiGameplay . uiModal
  case modal of
    Nothing -> openModal mt
    Just _ -> uiState . uiGameplay . uiModal .= Nothing >> safeAutoUnpause

setFocus :: FocusablePanel -> EventM Name AppState ()
setFocus name = uiState . uiGameplay . uiFocusRing %= focusSetCurrent (FocusablePanel name)

immediatelyRedrawWorld :: EventM Name AppState ()
immediatelyRedrawWorld = do
  invalidateCacheEntry WorldCache
  loadVisibleRegion

-- | Make sure all tiles covering the visible part of the world are
--   loaded.
loadVisibleRegion :: EventM Name AppState ()
loadVisibleRegion = do
  mext <- lookupExtent WorldExtent
  forM_ mext $ \(Extent _ _ size) -> do
    gs <- use gameState
    let vr = viewingRegion (gs ^. robotInfo . viewCenter) (over both fromIntegral size)
    gameState . landscape . multiWorld %= M.adjust (W.loadRegion (vr ^. planar)) (vr ^. subworld)

mouseLocToWorldCoords :: Brick.Location -> EventM Name GameState (Maybe (Cosmic Coords))
mouseLocToWorldCoords (Brick.Location mouseLoc) = do
  mext <- lookupExtent WorldExtent
  case mext of
    Nothing -> pure Nothing
    Just ext -> do
      region <- gets $ flip viewingRegion (bimap fromIntegral fromIntegral (extentSize ext)) . view (robotInfo . viewCenter)
      let regionStart = unCoords (fst $ region ^. planar)
          mouseLoc' = bimap fromIntegral fromIntegral mouseLoc
          mx = snd mouseLoc' + fst regionStart
          my = fst mouseLoc' + snd regionStart
       in pure . Just $ Cosmic (region ^. subworld) $ Coords (mx, my)

hasDebugCapability :: Bool -> AppState -> Bool
hasDebugCapability isCreative s =
  maybe isCreative (S.member CDebug . getCapabilitySet) $
    s ^? gameState . to focusedRobot . _Just . robotCapabilities

-- | Resets the viewport scroll position
resetViewport :: ViewportScroll Name -> EventM Name AppState ()
resetViewport n = do
  vScrollToBeginning n
  hScrollToBeginning n

-- | Modifies the game state using a fused-effect state action.
zoomGameState :: (MonadState AppState m, MonadIO m) => Fused.StateC GameState (TimeIOC (Fused.LiftC IO)) a -> m a
zoomGameState f = do
  gs <- use gameState
  (gs', a) <- liftIO (Fused.runM (runTimeIO (Fused.runState gs f)))
  gameState .= gs'
  return a

onlyCreative :: (MonadState AppState m) => m () -> m ()
onlyCreative a = do
  c <- use $ gameState . creativeMode
  when c a
