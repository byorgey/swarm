-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}

module Swarm.Game.Context (
  Ctx,
  VarCtx (..),
  Phase,
  Context (..),
) where

import Control.Applicative
import Data.Data
import Data.Functor.Identity
import Data.Map
import Data.Set
import Swarm.Game.Value
import Swarm.Language.Capability
import Swarm.Language.Types

-- | A context is a mapping from variable names to things.
newtype Ctx t = Ctx {unCtx :: Map Var t}
  deriving (Eq, Show, Functor, Foldable, Traversable, Data)

-- | Different phases of the program
data Phase
  = -- | Command parsing
    ParseCommand
  | -- | Checking capabilities for a definition
    CapabilityCheck
  | -- | Evaluating a Term
    EvaluateTerm
  deriving (Eq, Ord, Show)

-- A context is a mapping from variable name to information
-- about the variable. There can be multiple "things" that
-- independently store information about the variable
-- they are bundled together in the record
newtype Context (p :: Phase) = Context (Map Var (VarCtx p))
data VarCtx p = VarCtx
  { -- Set of capaibilities required to compute a variable
    varCaps :: CapsForPhase p (Set Capability)
  , -- Type of the value the variable stores
    varType :: TypeForPhase p Polytype
  , -- Value of the variable
    varVal :: ValForPhase p Value
  }

type family CapsForPhase (p :: Phase) :: * -> * where
  CapsForPhase 'ParseCommand = Maybe
  CapsForPhase 'CapabilityCheck = Identity
  -- capabilities need not be accessed when
  -- evaluating a term even if they already exist
  CapsForPhase 'EvaluateTerm = Const ()

type family TypeForPhase (p :: Phase) :: * -> * where
  TypeForPhase 'ParseCommand = Identity
  TypeForPhase 'CapabilityCheck = Const ()
  TypeForPhase 'EvaluateTerm = Const ()

type family ValForPhase (p :: Phase) :: * -> * where
  ValForPhase 'ParseCommand = Maybe
  ValForPhase 'CapabilityCheck = Maybe
  ValForPhase 'EvaluateTerm = Identity
