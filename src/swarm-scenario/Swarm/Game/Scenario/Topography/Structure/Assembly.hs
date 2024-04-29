{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Definitions of "structures" for use within a map
-- as well as logic for combining them.
module Swarm.Game.Scenario.Topography.Structure.Assembly where

import Control.Applicative ((<|>))
import Control.Arrow (left, (&&&))
import Control.Monad (when)
import Data.Coerce
import Data.Either.Extra (maybeToEither)
import Data.Foldable (foldrM)
import Data.Map qualified as M
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Game.Location
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.Navigation.Waypoint
import Swarm.Game.Scenario.Topography.Placement
import Swarm.Game.Scenario.Topography.Structure
import Swarm.Language.Direction (directionJsonModifier)
import Swarm.Util (commaList, quote, showT)

-- | Destructively overlays one direct child structure
-- upon the input structure.
-- However, the child structure is assembled recursively.
overlaySingleStructure ::
  M.Map StructureName (NamedStructure (Maybe a)) ->
  Placed (Maybe a) ->
  MergedStructure (Maybe a) ->
  Either Text (MergedStructure (Maybe a))
overlaySingleStructure
  inheritedStrucDefs
  (Placed p@(Placement _ loc@(Location colOffset rowOffset) orientation) ns)
  (MergedStructure inputArea inputPlacements inputWaypoints) = do
    MergedStructure overlayArea overlayPlacements overlayWaypoints <-
      mergeStructures inheritedStrucDefs (WithParent p) $ structure ns

    let mergedWaypoints = inputWaypoints <> map (fmap $ placeOnArea overlayArea) overlayWaypoints
        mergedPlacements = inputPlacements <> map (placeOnArea overlayArea) overlayPlacements
        mergedArea = zipWithPad mergeSingleRow inputArea $ paddedOverlayRows overlayArea

    return $ MergedStructure mergedArea mergedPlacements mergedWaypoints
   where
    placeOnArea overArea =
      offsetLoc (coerce loc)
        . modifyLoc (reorientLandmark orientation $ getAreaDimensions overArea)

    zipWithPad f a b = zipWith f a $ b <> repeat Nothing

    affineTransformedOverlay = applyOrientationTransform orientation

    mergeSingleRow inputRow maybeOverlayRow =
      zipWithPad (flip (<|>)) inputRow paddedSingleOverlayRow
     where
      paddedSingleOverlayRow = maybe [] (applyOffset colOffset) maybeOverlayRow

    paddedOverlayRows = applyOffset (negate rowOffset) . map Just . affineTransformedOverlay
    applyOffset offsetNum = modifyFront
     where
      integralOffset = fromIntegral offsetNum
      modifyFront =
        if integralOffset >= 0
          then (replicate integralOffset Nothing <>)
          else drop $ abs integralOffset

elaboratePlacement :: Parentage Placement -> Either Text a -> Either Text a
elaboratePlacement p = left (elaboration <>)
 where
  pTxt = case p of
    Root -> "root placement"
    WithParent (Placement (StructureName sn) loc _) ->
      T.unwords
        [ "placement of"
        , quote sn
        , "at"
        , showT loc
        ]
  elaboration =
    T.unwords
      [ "Within"
      , pTxt <> ":"
      , ""
      ]

-- | Overlays all of the "child placements", such that the children encountered earlier
-- in the YAML file supersede the later ones (due to use of 'foldr' instead of 'foldl').
mergeStructures ::
  M.Map StructureName (NamedStructure (Maybe a)) ->
  Parentage Placement ->
  PStructure (Maybe a) ->
  Either Text (MergedStructure (Maybe a))
mergeStructures inheritedStrucDefs parentPlacement (Structure origArea subStructures subPlacements subWaypoints) = do
  overlays <- elaboratePlacement parentPlacement $ mapM g subPlacements
  let wrapPlacement (Placed z ns) = LocatedStructure (name ns) (up $ orient z) $ offset z
      wrappedOverlays = map wrapPlacement $ filter (\(Placed _ ns) -> isRecognizable ns) overlays
  foldrM
    (overlaySingleStructure structureMap)
    (MergedStructure origArea wrappedOverlays originatedWaypoints)
    overlays
 where
  originatedWaypoints = map (Originated parentPlacement) subWaypoints

  -- deeper definitions override the outer (toplevel) ones
  structureMap = M.union (M.fromList $ map (name &&& id) subStructures) inheritedStrucDefs

  g placement@(Placement sName@(StructureName n) _ orientation) = do
    t@(_, ns) <-
      maybeToEither
        (T.unwords ["Could not look up structure", quote n])
        $ sequenceA (placement, M.lookup sName structureMap)
    let placementDirection = up orientation
        recognizedOrientations = recognize ns
    when (isRecognizable ns) $ do
      when (flipped orientation) $
        Left $
          T.unwords
            [ "Placing recognizable structure"
            , quote n
            , "with flipped orientation is not supported."
            ]

      -- Redundant orientations by rotational symmetry are accounted
      -- for at scenario parse time
      when (Set.notMember placementDirection recognizedOrientations) $
        Left $
          T.unwords
            [ "Placing recognizable structure"
            , quote n
            , "with"
            , renderDir placementDirection
            , "orientation is not supported."
            , "Try"
            , commaList $ map renderDir $ Set.toList recognizedOrientations
            , "instead."
            ]
    return $ uncurry Placed t
   where
    renderDir = quote . T.pack . directionJsonModifier . show
