{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utility functions
module TestUtil where

import Control.Lens (Ixed (ix), to, use, (&), (.~), (^.), (^?))
import Control.Monad (void)
import Control.Monad.State (StateT (..), execState)
import Control.Monad.Trans (lift)
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Effect
import Swarm.Game.CESK
import Swarm.Game.Exception
import Swarm.Game.Land
import Swarm.Game.Robot
import Swarm.Game.Robot.Concrete (isActive)
import Swarm.Game.State
import Swarm.Game.State.Landscape
import Swarm.Game.State.Robot
import Swarm.Game.Step (gameTick, hypotheticalRobot, stepCESK)
import Swarm.Language.Context
import Swarm.Language.Pipeline (ProcessedTerm (..), processTerm)
import Swarm.Language.Value
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure)
import Witch (into)

eval :: GameState -> Text -> IO (GameState, Robot, Either Text (Value, Int))
eval g = either (return . (g,hypotheticalRobot undefined 0,) . Left) (evalPT g) . processTerm1

processTerm1 :: Text -> Either Text ProcessedTerm
processTerm1 txt = processTerm txt >>= maybe wsErr Right
 where
  wsErr = Left "expecting a term, but got only whitespace"

evalPT :: GameState -> ProcessedTerm -> IO (GameState, Robot, Either Text (Value, Int))
evalPT g t = evalCESK g (initMachine t empty emptyStore)

evalCESK :: GameState -> CESK -> IO (GameState, Robot, Either Text (Value, Int))
evalCESK g cesk =
  runCESK 0 cesk
    & flip runStateT r
    & flip runStateT (g & creativeMode .~ True)
    & fmap orderResult
 where
  r = hypotheticalRobot cesk 0
  orderResult ((res, rr), rg) = (rg, rr, res)

runCESK :: Int -> CESK -> StateT Robot (StateT GameState IO) (Either Text (Value, Int))
runCESK _ (Up exn _ []) = Left . flip formatExn exn <$> lift (use $ landscape . terrainAndEntities . entityMap)
runCESK !steps cesk = case finalValue cesk of
  Just (v, _) -> return (Right (v, steps))
  Nothing -> runTimeIO (stepCESK cesk) >>= runCESK (steps + 1)

play :: GameState -> Text -> IO (Either Text (), GameState)
play g = either (return . (,g) . Left) playPT . processTerm1
 where
  playPT pt = runStateT (playUntilDone (hr ^. robotID)) gs
   where
    cesk = initMachine pt empty emptyStore
    hr = hypotheticalRobot cesk 0
    hid = hr ^. robotID
    gs =
      g
        & execState (zoomRobots $ addRobot hr)
        & robotInfo . viewCenterRule .~ VCRobot hid
        & creativeMode .~ True

playUntilDone :: RID -> StateT GameState IO (Either Text ())
playUntilDone rid = do
  w <- use $ robotInfo . robotMap
  case w ^? ix rid . to isActive of
    Just True -> do
      void $ runTimeIO gameTick
      playUntilDone rid
    Just False -> return $ Right ()
    Nothing -> return $ Left . T.pack $ "The robot with ID " <> show rid <> " is nowhere to be found!"

check :: Text -> (ProcessedTerm -> Bool) -> Assertion
check code expect = case processTerm1 code of
  Left err -> assertFailure $ "Term processing failed: " ++ into @String err
  Right pt -> assertBool "Predicate was false!" (expect pt)
