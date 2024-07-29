{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.WorldDescription where

import Control.Applicative ((<|>))
import Control.Carrier.Reader (runReader)
import Control.Carrier.Throw.Either
import Control.Monad (forM)
import Data.Coerce
import Data.Functor.Identity
import Data.Text qualified as T
import Data.Yaml as Y
import Swarm.Game.Entity (Entity)
import Swarm.Game.Land
import Swarm.Game.Location
import Swarm.Game.Scenario.RobotLookup (RobotMap)
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.EntityFacade (EntityFacade)
import Swarm.Game.Scenario.Topography.Navigation.Portal
import Swarm.Game.Scenario.Topography.Navigation.Waypoint (
  Parentage (Root),
  WaypointName,
 )
import Swarm.Game.Scenario.Topography.ProtoCell (
  StructurePalette (StructurePalette),
 )
import Swarm.Game.Scenario.Topography.Structure (
  LocatedStructure,
  MergedStructure (MergedStructure),
  NamedStructure,
  parseStructure,
 )
import Swarm.Game.Scenario.Topography.Structure.Assembly qualified as Assembly
import Swarm.Game.Scenario.Topography.Structure.Overlay (
  PositionedGrid (..),
 )
import Swarm.Game.Scenario.Topography.WorldPalette
import Swarm.Game.Universe (SubworldName (DefaultRootSubworld))
import Swarm.Game.World.Parse ()
import Swarm.Game.Scenario.Topography.Placement
import Swarm.Game.World.Syntax
import Swarm.Game.Terrain (TerrainType)
import Swarm.Game.World.Typecheck
import Swarm.Language.Pretty (prettyString)
import Swarm.Util.Yaml

------------------------------------------------------------
-- World description
------------------------------------------------------------


type SpecialPalette e = StructurePalette (CellContent (PCell e))


-- | A description of a world parsed from a YAML file.
-- This type is parameterized to accommodate Cells that
-- utilize a less stateful Entity type.
data PWorldDescription e = WorldDescription
  { offsetOrigin :: Bool
  , scrollable :: Bool
  -- , palette :: WorldPalette e
  , palette :: SpecialPalette e
  , area :: PositionedGrid (Maybe (PCell e))
  , navigation :: Navigation Identity WaypointName
  , placedStructures :: [LocatedStructure]
  -- ^ statically-placed structures to pre-populate
  -- the structure recognizer
  , worldName :: SubworldName
  , worldProg :: Maybe (TTerm '[] (World CellVal))
  }
  deriving (Show)

type WorldDescription = PWorldDescription Entity

type InheritedStructureDefs = [NamedStructure (Maybe Cell)]

data WorldParseDependencies
  = WorldParseDependencies
      WorldMap
      InheritedStructureDefs
      RobotMap
      -- | last for the benefit of partial application
      TerrainEntityMaps

instance FromJSONE WorldParseDependencies WorldDescription where
  parseJSONE = withObjectE "world description" $ \v -> do
    WorldParseDependencies worldMap scenarioLevelStructureDefs rm tem <- getE

    let withDeps = localE (const (tem, rm))
    palette <-
      withDeps $
        v ..:? "palette" ..!= StructurePalette mempty
    subworldLocalStructureDefs <-
      withDeps $
        v ..:? "structures" ..!= []

    let initialStructureDefs = scenarioLevelStructureDefs <> subworldLocalStructureDefs
    liftE $ mkWorld tem worldMap palette initialStructureDefs v
   where
    mkWorld :: TerrainEntityMaps
      -> WorldMap
      -> SpecialPalette (PCell e)
      -> [NamedStructure (Maybe (PCell e))]
      -> Object
      -> Parser (PWorldDescription e)
    mkWorld tem worldMap palette initialStructureDefs v = do
      MergedStructure mergedGrid staticStructurePlacements unmergedWaypoints <- do
        unflattenedStructure <- parseStructure palette initialStructureDefs v
        either (fail . T.unpack) return $
          Assembly.mergeStructures mempty Root unflattenedStructure

      worldName <- v .:? "name" .!= DefaultRootSubworld
      ul <- v .:? "upperleft" .!= origin
      portalDefs <- v .:? "portals" .!= []
      navigation <-
        validatePartialNavigation
          worldName
          ul
          unmergedWaypoints
          portalDefs

      mwexp <- v .:? "dsl"
      worldProg <- forM mwexp $ \wexp -> do
        let checkResult =
              run . runThrow @CheckErr . runReader worldMap . runReader tem $
                check CNil (TTyWorld TTyCell) wexp
        either (fail . prettyString) return checkResult

      offsetOrigin <- v .:? "offset" .!= False
      scrollable <- v .:? "scrollable" .!= True
      let placedStructures =
            map (offsetLoc $ coerce ul) staticStructurePlacements

      -- Override upper-left corner with explicit location
      let area = mergedGrid {gridPosition = ul}

      return $ WorldDescription {..}

------------------------------------------------------------
-- World editor
------------------------------------------------------------

-- | A pared-down (stateless) version of "WorldDescription" just for
-- the purpose of rendering a Scenario file
type WorldDescriptionPaint = PWorldDescription EntityFacade

instance ToJSON WorldDescriptionPaint where
  toJSON w =
    object
      [ "offset" .= offsetOrigin w
      , "palette" .= Y.toJSON paletteKeymap
      , "upperleft" .= gridPosition (area w)
      , "map" .= Y.toJSON mapText
      ]
   where
    cellGrid = gridContent $ area w
    suggestedPalette = PaletteAndMaskChar (palette w) Nothing
    (mapText, paletteKeymap) = prepForJson suggestedPalette cellGrid
