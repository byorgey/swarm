-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Online operations for structure recognizer.
--
-- See "Swarm.Game.Scenario.Topography.Structure.Recognition.Precompute" for
-- details of the structure recognition process.
module Swarm.Game.Scenario.Topography.Structure.Recognition.Tracking (
  entityModified,
) where

import Control.Carrier.State.Lazy
import Control.Effect.Lens
import Control.Lens ((^.))
import Control.Monad (forM, forM_)
import Data.Hashable (Hashable)
import Data.Int (Int32)
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Data.Semigroup (Max (..), Min (..))
import Linear (V2 (..))
import Swarm.Game.Entity
import Swarm.Game.Location
import Swarm.Game.Scenario.Topography.Structure qualified as Structure
import Swarm.Game.Scenario.Topography.Structure.Recognition
import Swarm.Game.Scenario.Topography.Structure.Recognition.Log
import Swarm.Game.Scenario.Topography.Structure.Recognition.Registry
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.State
import Swarm.Game.Universe
import Swarm.Game.World.Modify
import Text.AhoCorasick

-- | A hook called from the centralized entity update function,
-- 'Swarm.Game.Step.Util.updateEntityAt'.
--
-- This handles structure detection upon addition of an entity,
-- and structure de-registration upon removal of an entity.
-- Also handles atomic entity swaps.
entityModified ::
  (Has (State GameState) sig m) =>
  CellModification Entity ->
  Cosmic Location ->
  m ()
entityModified modification cLoc = do
  case modification of
    Add newEntity -> doAddition newEntity
    Remove _ -> doRemoval
    Swap _ newEntity -> doRemoval >> doAddition newEntity
 where
  doAddition newEntity = do
    entLookup <- use $ discovery . structureRecognition . automatons . automatonsByEntity
    forM_ (M.lookup newEntity entLookup) $ \finder -> do
      let msg = FoundParticipatingEntity $ ParticipatingEntity (view entityName newEntity) (finder ^. inspectionOffsets)
      discovery . structureRecognition . recognitionLog %= (msg :)
      registerRowMatches cLoc finder

  doRemoval = do
    -- Entity was removed; may need to remove registered structure.
    structureRegistry <- use $ discovery . structureRecognition . foundStructures
    forM_ (M.lookup cLoc $ foundByLocation structureRegistry) $ \fs -> do
      let structureName = Structure.name $ originalDefinition $ structureWithGrid fs
       in do
            discovery . structureRecognition . recognitionLog %= (StructureRemoved structureName :)
            discovery . structureRecognition . foundStructures %= removeStructure fs

-- | Ensures that the entity in this cell is not already
-- participating in a registered structure
availableEntityAt ::
  (Has (State GameState) sig m) =>
  Cosmic Location ->
  m (Maybe Entity)
availableEntityAt cLoc = do
  registry <- use $ discovery . structureRecognition . foundStructures
  if M.member cLoc $ foundByLocation registry
    then return Nothing
    else entityAt cLoc

-- | Excludes entities that are already part of a
-- registered found structure.
getWorldRow ::
  (Has (State GameState) sig m) =>
  Cosmic Location ->
  InspectionOffsets ->
  Int32 ->
  m [Maybe Entity]
getWorldRow cLoc (InspectionOffsets (Min offsetLeft) (Max offsetRight)) yOffset =
  mapM availableEntityAt horizontalOffsets
 where
  horizontalOffsets = map mkLoc [offsetLeft .. offsetRight]

  -- NOTE: We negate the yOffset because structure rows are numbered increasing from top
  -- to bottom, but swarm world coordinates increase from bottom to top.
  mkLoc x = cLoc `offsetBy` V2 x (negate yOffset)

registerRowMatches ::
  (Has (State GameState) sig m) =>
  Cosmic Location ->
  AutomatonInfo AtomicKeySymbol StructureSearcher ->
  m ()
registerRowMatches cLoc (AutomatonInfo horizontalOffsets sm) = do
  entitiesRow <- getWorldRow cLoc horizontalOffsets 0
  let candidates = findAll sm entitiesRow
      mkCandidateLogEntry c =
        FoundRowCandidate
          (HaystackContext (map (fmap $ view entityName) entitiesRow) (HaystackPosition $ pIndex c))
          (map (fmap $ view entityName) . needleContent $ pVal c)
          rowMatchInfo
       where
        rowMatchInfo = NE.toList . NE.map (f . myRow) . singleRowItems $ pVal c
         where
          f x = MatchingRowFrom (rowIndex x) $ Structure.name . originalDefinition . wholeStructure $ x

      logEntry = FoundRowCandidates $ map mkCandidateLogEntry candidates

  discovery . structureRecognition . recognitionLog %= (logEntry :)
  candidates2D <- forM candidates $ checkVerticalMatch cLoc horizontalOffsets
  registerStructureMatches $ concat candidates2D

checkVerticalMatch ::
  (Has (State GameState) sig m) =>
  Cosmic Location ->
  -- | Horizontal search offsets
  InspectionOffsets ->
  Position StructureSearcher ->
  m [FoundStructure]
checkVerticalMatch cLoc (InspectionOffsets (Min searchOffsetLeft) _) foundRow =
  getMatches2D cLoc horizontalFoundOffsets $ automaton2D $ pVal foundRow
 where
  foundLeftOffset = searchOffsetLeft + fromIntegral (pIndex foundRow)
  foundRightInclusiveIndex = foundLeftOffset + fromIntegral (pLength foundRow) - 1
  horizontalFoundOffsets = InspectionOffsets (pure foundLeftOffset) (pure foundRightInclusiveIndex)

getFoundStructures ::
  Hashable keySymb =>
  (Int32, Int32) ->
  Cosmic Location ->
  StateMachine keySymb StructureRow ->
  [keySymb] ->
  [FoundStructure]
getFoundStructures (offsetTop, offsetLeft) cLoc sm entityRows =
  map mkFound candidates
 where
  candidates = findAll sm entityRows
  mkFound candidate = FoundStructure (wholeStructure $ pVal candidate) $ cLoc `offsetBy` loc
   where
    -- NOTE: We negate the yOffset because structure rows are numbered increasing from top
    -- to bottom, but swarm world coordinates increase from bottom to top.
    loc = V2 offsetLeft $ negate $ offsetTop + fromIntegral (pIndex candidate)

getMatches2D ::
  (Has (State GameState) sig m) =>
  Cosmic Location ->
  -- | Horizontal found offsets (inclusive indices)
  InspectionOffsets ->
  AutomatonInfo SymbolSequence StructureRow ->
  m [FoundStructure]
getMatches2D
  cLoc
  horizontalFoundOffsets@(InspectionOffsets (Min offsetLeft) _)
  (AutomatonInfo (InspectionOffsets (Min offsetTop) (Max offsetBottom)) sm) = do
    entityRows <- mapM getRow verticalOffsets
    return $ getFoundStructures (offsetTop, offsetLeft) cLoc sm entityRows
   where
    getRow = getWorldRow cLoc horizontalFoundOffsets
    verticalOffsets = [offsetTop .. offsetBottom]

-- |
-- We only allow an entity to participate in one structure at a time,
-- so multiple matches require a tie-breaker.
-- The largest structure (by area) shall win.
registerStructureMatches ::
  (Has (State GameState) sig m) =>
  [FoundStructure] ->
  m ()
registerStructureMatches unrankedCandidates = do
  discovery . structureRecognition . recognitionLog %= (newMsg :)

  forM_ (listToMaybe rankedCandidates) $ \fs ->
    discovery . structureRecognition . foundStructures %= addFound fs
 where
  -- Sorted by decreasing order of preference.
  rankedCandidates = sortOn Down unrankedCandidates

  getStructureName (FoundStructure swg _) = Structure.name $ originalDefinition swg
  newMsg = FoundCompleteStructureCandidates $ map getStructureName rankedCandidates
