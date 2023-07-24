-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Abstract syntax for the Swarm world description DSL.
module Swarm.Game.World.Syntax (
  -- | Various component types
  World,
  RawCellVal,
  CellTag (..),
  CellVal (..),
  Rot (..),
  Var,
  Axis (..),
  Op (..),
  -- | The main AST type
  WExp (..),
)
where

import Data.List.NonEmpty qualified as NE
import Data.Semigroup (Last (..))
import Data.Text (Text)
import Swarm.Game.Entity (Entity)
import Swarm.Game.Robot (Robot)
import Swarm.Game.Terrain
import Swarm.Game.World.Coords
import Swarm.Util.Erasable

------------------------------------------------------------
-- Bits and bobs

type World b = Coords -> b

data CellTag = CellTerrain | CellEntity | CellRobot
  deriving (Eq, Ord, Show, Enum, Bounded)

type RawCellVal = [(Maybe CellTag, Text)]

data CellVal = CellVal TerrainType (Erasable (Last Entity)) [Robot]
  deriving (Eq, Show)

data Rot = Rot0 | Rot90 | Rot180 | Rot270
  deriving (Eq, Ord, Show, Bounded, Enum)

type Var = Text

data Axis = X | Y
  deriving (Eq, Ord, Show, Bounded, Enum)

data Op = Not | Neg | And | Or | Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Leq | Gt | Geq | If | Perlin | Reflect Axis | Rot Rot | Mask | Overlay | Abs
  deriving (Eq, Ord, Show)

------------------------------------------------------------
-- Main AST

data WExp where
  WInt :: Integer -> WExp
  WFloat :: Double -> WExp
  WBool :: Bool -> WExp
  WCell :: RawCellVal -> WExp
  WVar :: Text -> WExp
  -- Require all operators to be fully saturated.  Just embedding
  -- operators as constants and including function application would
  -- be a more elegant encoding, but it requires being more clever
  -- with type inference.
  WOp :: Op -> [WExp] -> WExp
  WSeed :: WExp
  WCoord :: Axis -> WExp
  WHash :: WExp
  WLet :: [(Var, WExp)] -> WExp -> WExp
  WOverlay :: NE.NonEmpty WExp -> WExp
  WImport :: Text -> WExp
  -- We'll add these later.  XXX
  -- WCat :: Axis -> [WExp] -> WExp
  -- WStruct :: WorldPalette Text -> [Text] -> WExp
  deriving (Eq, Show)

-- We don't have an explicit Empty case because we can't infer its
-- type.  It could be done but it would require a lot more care with
-- inference vs checking mode.
