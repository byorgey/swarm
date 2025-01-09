{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Robot-specific subrecords and utilities used by 'Swarm.Game.State.GameState'
module Swarm.Game.State.Robot (
  -- * Types
  ViewCenterRule (..),
  Robots,

  -- * Robot naming
  RobotNaming,
  nameGenerator,
  gensym,
  robotNaming,

  -- * Initialization
  initRobots,
  setRobotInfo,

  -- * Accessors
  robotMap,
  robotsByLocation,
  robotsWatching,
  activeRobots,
  waitingRobots,
  currentTickWakeableBots,
  viewCenterRule,
  viewCenter,
  focusedRobotID,

  -- * Utilities
  wakeWatchingRobots,
  sleepUntil,
  sleepForever,
  wakeUpRobotsDoneSleeping,
  deleteRobot,
  removeRobotFromLocationMap,
  activateRobot,
  addRobot,
  addRobotToLocation,
  addTRobot,
  addTRobot',

  -- ** View
  modifyViewCenter,
  unfocus,
  recalcViewCenter,
) where

import Control.Arrow (Arrow ((&&&)))
import Control.Effect.Lens
import Control.Effect.State (State)
import Control.Effect.Throw (Has)
import Control.Lens hiding (Const, use, uses, view, (%=), (+=), (.=), (<+=), (<<.=))
import Control.Monad (forM_, void)
import Data.Aeson (FromJSON, ToJSON)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.IntSet.Lens (setOf)
import Data.List (partition)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as S
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Swarm.Game.CESK (CESK (Waiting))
import Swarm.Game.Location
import Swarm.Game.Robot
import Swarm.Game.Robot.Concrete
import Swarm.Game.State.Config
import Swarm.Game.Tick
import Swarm.Game.Universe as U
import Swarm.ResourceLoading (NameGenerator)
import Swarm.Util (binTuples, surfaceEmpty, (<+=), (<<.=))
import Swarm.Util.Lens (makeLensesExcluding)

-- | The 'ViewCenterRule' specifies how to determine the center of the
--   world viewport.
data ViewCenterRule
  = -- | The view should be centered on an absolute position.
    VCLocation (Cosmic Location)
  | -- | The view should be centered on a certain robot.
    VCRobot RID
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

makePrisms ''ViewCenterRule

data RobotNaming = RobotNaming
  { _nameGenerator :: NameGenerator
  , _gensym :: Int
  }

makeLensesExcluding ['_nameGenerator] ''RobotNaming

--- | Read-only list of words, for use in building random robot names.
nameGenerator :: Getter RobotNaming NameGenerator
nameGenerator = to _nameGenerator

-- | A counter used to generate globally unique IDs.
gensym :: Lens' RobotNaming Int

data Robots = Robots
  { _robotMap :: IntMap Robot
  , -- A set of robots to consider for the next game tick. It is guaranteed to
    -- be a subset of the keys of 'robotMap'. It may contain waiting or idle
    -- robots. But robots that are present in 'robotMap' and not in 'activeRobots'
    -- are guaranteed to be either waiting or idle.
    _activeRobots :: IntSet
  , -- A set of probably waiting robots, indexed by probable wake-up time. It
    -- may contain robots that are in fact active or idle, as well as robots
    -- that do not exist anymore. Its only guarantee is that once a robot name
    -- with its wake up time is inserted in it, it will remain there until the
    -- wake-up time is reached, at which point it is removed via
    -- 'wakeUpRobotsDoneSleeping'.
    -- Waiting robots for a given time are a list because it is cheaper to
    -- prepend to a list than insert into a 'Set'.
    _waitingRobots :: Map TickNumber [RID]
  , _currentTickWakeableBots :: [RID]
  , _robotsByLocation :: Map SubworldName (Map Location IntSet)
  , -- This member exists as an optimization so
    -- that we do not have to iterate over all "waiting" robots,
    -- since there may be many.
    _robotsWatching :: Map (Cosmic Location) IntSet
  , _robotNaming :: RobotNaming
  , _viewCenterRule :: ViewCenterRule
  , _viewCenter :: Cosmic Location
  , _focusedRobotID :: RID
  }

-- We want to access active and waiting robots via lenses inside
-- this module but to expose it as a Getter to protect invariants.
makeLensesFor
  [ ("_activeRobots", "internalActiveRobots")
  , ("_waitingRobots", "internalWaitingRobots")
  ]
  ''Robots

makeLensesExcluding ['_viewCenter, '_viewCenterRule, '_focusedRobotID, '_activeRobots, '_waitingRobots] ''Robots

-- | All the robots that currently exist in the game, indexed by ID.
robotMap :: Lens' Robots (IntMap Robot)

-- | The names of the robots that are currently not sleeping.
activeRobots :: Getter Robots IntSet
activeRobots = internalActiveRobots

-- | The names of the robots that are currently sleeping, indexed by wake up
--   time. Note that this may not include all sleeping robots, particularly
--   those that are only taking a short nap (e.g. @wait 1@).
waitingRobots :: Getter Robots (Map TickNumber [RID])
waitingRobots = internalWaitingRobots

-- | Get a list of all the robots that are \"watching\" by location.
currentTickWakeableBots :: Lens' Robots [RID]

-- | The names of all robots that currently exist in the game, indexed by
--   location (which we need both for /e.g./ the @salvage@ command as
--   well as for actually drawing the world).  Unfortunately there is
--   no good way to automatically keep this up to date, since we don't
--   just want to completely rebuild it every time the 'robotMap'
--   changes.  Instead, we just make sure to update it every time the
--   location of a robot changes, or a robot is created or destroyed.
--   Fortunately, there are relatively few ways for these things to
--   happen.
robotsByLocation :: Lens' Robots (Map SubworldName (Map Location IntSet))

-- | Get a list of all the robots that are \"watching\" by location.
robotsWatching :: Lens' Robots (Map (Cosmic Location) IntSet)

-- | State and data for assigning identifiers to robots
robotNaming :: Lens' Robots RobotNaming

-- | The current center of the world view. Note that this cannot be
--   modified directly, since it is calculated automatically from the
--   'viewCenterRule'.  To modify the view center, either set the
--   'viewCenterRule', or use 'modifyViewCenter'.
viewCenter :: Getter Robots (Cosmic Location)
viewCenter = to _viewCenter

-- | The current robot in focus.
--
-- It is only a 'Getter' because this value should be updated only when
-- the 'viewCenterRule' is specified to be a robot.
--
-- Technically it's the last robot ID specified by 'viewCenterRule',
-- but that robot may not be alive anymore - to be safe use 'focusedRobot'.
focusedRobotID :: Getter Robots RID
focusedRobotID = to _focusedRobotID

-- * Utilities

initRobots :: GameStateConfig -> Robots
initRobots gsc =
  Robots
    { _robotMap = IM.empty
    , _activeRobots = IS.empty
    , _waitingRobots = M.empty
    , _currentTickWakeableBots = mempty
    , _robotsByLocation = M.empty
    , _robotsWatching = mempty
    , _robotNaming =
        RobotNaming
          { _nameGenerator = nameParts gsc
          , _gensym = 0
          }
    , _viewCenterRule = VCRobot 0
    , _viewCenter = defaultCosmicLocation
    , _focusedRobotID = 0
    }

-- | The current rule for determining the center of the world view.
--   It updates also, 'viewCenter' and 'focusedRobot' to keep
--   everything synchronized.
viewCenterRule :: Lens' Robots ViewCenterRule
viewCenterRule = lens getter setter
 where
  getter :: Robots -> ViewCenterRule
  getter = _viewCenterRule

  -- The setter takes care of updating 'viewCenter' and 'focusedRobot'
  -- So none of these fields get out of sync.
  setter :: Robots -> ViewCenterRule -> Robots
  setter g rule =
    case rule of
      VCLocation loc -> g {_viewCenterRule = rule, _viewCenter = loc}
      VCRobot rid ->
        let robotcenter = g ^? robotMap . ix rid . robotLocation
         in -- retrieve the loc of the robot if it exists, Nothing otherwise.
            -- sometimes, lenses are amazing...
            case robotcenter of
              Nothing -> g
              Just loc -> g {_viewCenterRule = rule, _viewCenter = loc, _focusedRobotID = rid}

-- | Add a concrete instance of a robot template to the game state:
--   First, generate a unique ID number for it.  Then, add it to the
--   main robot map, the active robot set, and to to the index of
--   robots by location.
addTRobot :: (Has (State Robots) sig m) => CESK -> TRobot -> m ()
addTRobot m r = void $ addTRobot' m r

-- | Like addTRobot, but return the newly instantiated robot.
addTRobot' :: (Has (State Robots) sig m) => CESK -> TRobot -> m Robot
addTRobot' initialMachine r = do
  rid <- robotNaming . gensym <+= 1
  let newRobot = instantiateRobot (Just initialMachine) rid r
  addRobot newRobot
  return newRobot

-- | Add a robot to the game state, adding it to the main robot map,
--   the active robot set, and to to the index of robots by
--   location.
addRobot :: (Has (State Robots) sig m) => Robot -> m ()
addRobot r = do
  robotMap %= IM.insert rid r
  addRobotToLocation rid $ r ^. robotLocation
  internalActiveRobots %= IS.insert rid
 where
  rid = r ^. robotID

-- | Helper function for updating the "robotsByLocation" bookkeeping
addRobotToLocation :: (Has (State Robots) sig m) => RID -> Cosmic Location -> m ()
addRobotToLocation rid rLoc =
  robotsByLocation
    %= M.insertWith
      (M.unionWith IS.union)
      (rLoc ^. subworld)
      (M.singleton (rLoc ^. planar) (IS.singleton rid))

-- | Takes a robot out of the 'activeRobots' set and puts it in the 'waitingRobots'
--   queue.
sleepUntil :: (Has (State Robots) sig m) => RID -> TickNumber -> m ()
sleepUntil rid time = do
  internalActiveRobots %= IS.delete rid
  internalWaitingRobots . at time . non [] %= (rid :)

-- | Takes a robot out of the 'activeRobots' set.
sleepForever :: (Has (State Robots) sig m) => RID -> m ()
sleepForever rid = internalActiveRobots %= IS.delete rid

-- | Adds a robot to the 'activeRobots' set.
activateRobot :: (Has (State Robots) sig m) => RID -> m ()
activateRobot rid = internalActiveRobots %= IS.insert rid

-- | Removes robots whose wake up time matches the current game ticks count
--   from the 'waitingRobots' queue and put them back in the 'activeRobots' set
--   if they still exist in the keys of 'robotMap'.
--
-- = Mutations
--
-- This function modifies:
--
-- * 'wakeLog'
-- * 'robotsWatching'
-- * 'internalWaitingRobots'
-- * 'internalActiveRobots' (aka 'activeRobots')
wakeUpRobotsDoneSleeping :: (Has (State Robots) sig m) => TickNumber -> m ()
wakeUpRobotsDoneSleeping time = do
  mrids <- internalWaitingRobots . at time <<.= Nothing
  forM_ mrids $ \rids -> do
    robots <- use robotMap
    let robotIdSet = IM.keysSet robots
        wakeableRIDsSet = IS.fromList rids

        -- Limit ourselves to the robots that have not expired in their sleep
        newlyAlive = IS.intersection robotIdSet wakeableRIDsSet

    internalActiveRobots %= IS.union newlyAlive

    -- These robots' wake times may have been moved "forward"
    -- by 'wakeWatchingRobots'.
    clearWatchingRobots wakeableRIDsSet

-- | Clear the "watch" state of all of the
-- awakened robots
clearWatchingRobots ::
  (Has (State Robots) sig m) =>
  IntSet ->
  m ()
clearWatchingRobots rids = do
  robotsWatching %= M.map (`IS.difference` rids)

-- | Iterates through all of the currently @wait@-ing robots,
-- and moves forward the wake time of the ones that are @watch@-ing this location.
--
-- NOTE: Clearing 'TickNumber' map entries from 'internalWaitingRobots'
-- upon wakeup is handled by 'wakeUpRobotsDoneSleeping'
wakeWatchingRobots :: (Has (State Robots) sig m) => RID -> TickNumber -> Cosmic Location -> m ()
wakeWatchingRobots myID currentTick loc = do
  waitingMap <- use waitingRobots
  rMap <- use robotMap
  watchingMap <- use robotsWatching

  -- The bookkeeping updates to robot waiting
  -- states are prepared in 4 steps...

  let -- Step 1: Identify the robots that are watching this location.
      botsWatchingThisLoc :: [Robot]
      botsWatchingThisLoc =
        mapMaybe (`IM.lookup` rMap) $
          IS.toList $
            M.findWithDefault mempty loc watchingMap

      -- Step 2: Get the target wake time for each of these robots
      wakeTimes :: [(RID, TickNumber)]
      wakeTimes = mapMaybe (sequenceA . (view robotID &&& waitingUntil)) botsWatchingThisLoc

      wakeTimesToPurge :: Map TickNumber (S.Set RID)
      wakeTimesToPurge = M.fromListWith (<>) $ map (fmap S.singleton . swap) wakeTimes

      -- Step 3: Take these robots out of their time-indexed slot in "waitingRobots".
      -- To preserve performance, this should be done without iterating over the
      -- entire "waitingRobots" map.
      filteredWaiting = foldr f waitingMap $ M.toList wakeTimesToPurge
       where
        -- Note: some of the map values may become empty lists.
        -- But we shall not worry about cleaning those up here;
        -- they will be "garbage collected" as a matter of course
        -- when their tick comes up in "wakeUpRobotsDoneSleeping".
        f (k, botsToRemove) = M.adjust (filter (`S.notMember` botsToRemove)) k

      -- Step 4: Re-add the watching bots to be awakened ASAP:
      wakeableBotIds = map fst wakeTimes

      -- It is crucial that only robots with a larger RID than the current robot
      -- be scheduled for the *same* tick, since within a given tick we iterate over
      -- robots in increasing order of RID.
      -- See note in 'iterateRobots'.
      (currTickWakeable, nextTickWakeable) = partition (> myID) wakeableBotIds
      wakeTimeGroups =
        [ (currentTick, currTickWakeable)
        , (addTicks 1 currentTick, nextTickWakeable)
        ]
      newInsertions = M.filter (not . null) $ M.fromList wakeTimeGroups

  -- Contract: This must be emptied immediately
  -- in 'iterateRobots'.
  --
  -- Tracking the wakeups due on the current tick separately from other
  -- waiting robots is a performance optimization (see #1736);
  -- it avoids an O(log N) 'Map' lookup on 'internalWaitingRobots' in favor
  -- of an O(1) 'null' check in the common case.
  currentTickWakeableBots .= currTickWakeable

  -- NOTE: There are two "sources of truth" for the waiting state of robots:
  -- 1. In the GameState via "internalWaitingRobots"
  -- 2. In each robot, via the CESK machine state

  -- 1. Update the game state
  internalWaitingRobots .= M.unionWith (<>) filteredWaiting newInsertions

  -- 2. Update the machine of each robot
  forM_ wakeTimeGroups $ \(newWakeTime, wakeableBots) ->
    forM_ wakeableBots $ \rid ->
      robotMap . at rid . _Just . machine %= \case
        Waiting _ c -> Waiting newWakeTime c
        x -> x

deleteRobot :: (Has (State Robots) sig m) => RID -> m ()
deleteRobot rn = do
  internalActiveRobots %= IS.delete rn
  mrobot <- robotMap . at rn <<.= Nothing
  mrobot `forM_` \robot -> do
    -- Delete the robot from the index of robots by location.
    removeRobotFromLocationMap (robot ^. robotLocation) rn

-- | Makes sure empty sets don't hang around in the
-- 'robotsByLocation' map.  We don't want a key with an
-- empty set at every location any robot has ever
-- visited!
removeRobotFromLocationMap ::
  (Has (State Robots) sig m) =>
  Cosmic Location ->
  RID ->
  m ()
removeRobotFromLocationMap (Cosmic oldSubworld oldPlanar) rid =
  robotsByLocation %= M.update (tidyDelete rid) oldSubworld
 where
  deleteOne x = surfaceEmpty IS.null . IS.delete x

  tidyDelete robID =
    surfaceEmpty M.null . M.update (deleteOne robID) oldPlanar

setRobotInfo :: RID -> [Robot] -> Robots -> Robots
setRobotInfo baseID robotList rState =
  (setRobotList robotList rState) {_focusedRobotID = baseID}
    & viewCenterRule .~ VCRobot baseID

setRobotList :: [Robot] -> Robots -> Robots
setRobotList robotList rState =
  rState
    & robotMap .~ IM.fromList (map (view robotID &&& id) robotList)
    & robotsByLocation .~ M.map (groupRobotsByPlanarLocation . NE.toList) (groupRobotsBySubworld robotList)
    & internalActiveRobots .~ setOf (traverse . robotID) robotList
    & robotNaming . gensym .~ initGensym
 where
  initGensym = length robotList - 1

  groupRobotsBySubworld =
    binTuples . map (view (robotLocation . subworld) &&& id)

  groupRobotsByPlanarLocation rs =
    M.fromListWith
      IS.union
      (map (view (robotLocation . planar) &&& (IS.singleton . view robotID)) rs)

-- | Modify the 'viewCenter' by applying an arbitrary function to the
--   current value.  Note that this also modifies the 'viewCenterRule'
--   to match.  After calling this function the 'viewCenterRule' will
--   specify a particular location, not a robot.
modifyViewCenter :: (Cosmic Location -> Cosmic Location) -> Robots -> Robots
modifyViewCenter update rInfo =
  rInfo
    & case rInfo ^. viewCenterRule of
      VCLocation l -> viewCenterRule .~ VCLocation (update l)
      VCRobot _ -> viewCenterRule .~ VCLocation (update (rInfo ^. viewCenter))

-- | "Unfocus" by modifying the view center rule to look at the
--   current location instead of a specific robot, and also set the
--   focused robot ID to an invalid value.  In classic mode this
--   causes the map view to become nothing but static.
unfocus :: Robots -> Robots
unfocus = (\ri -> ri {_focusedRobotID = -1000}) . modifyViewCenter id

-- | Recalculate the view center (and cache the result in the
--   'viewCenter' field) based on the current 'viewCenterRule'.  If
--   the 'viewCenterRule' specifies a robot which does not exist,
--   simply leave the current 'viewCenter' as it is.
recalcViewCenter :: Robots -> Robots
recalcViewCenter rInfo =
  rInfo
    { _viewCenter = newViewCenter
    }
 where
  newViewCenter =
    fromMaybe (rInfo ^. viewCenter) $
      applyViewCenterRule (rInfo ^. viewCenterRule) (rInfo ^. robotMap)

-- | Given a current mapping from robot names to robots, apply a
--   'ViewCenterRule' to derive the location it refers to.  The result
--   is 'Maybe' because the rule may refer to a robot which does not
--   exist.
applyViewCenterRule :: ViewCenterRule -> IntMap Robot -> Maybe (Cosmic Location)
applyViewCenterRule (VCLocation l) _ = Just l
applyViewCenterRule (VCRobot name) m = m ^? at name . _Just . robotLocation
