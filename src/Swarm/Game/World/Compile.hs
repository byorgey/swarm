{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Compiling Swarm world description DSL to native Haskell terms, via
-- combinators.
--
-- XXX for more info see ...
module Swarm.Game.World.Compile where

import Data.Kind (Type)
import Swarm.Game.Location (pattern Location)
import Swarm.Game.World.Coords (coordsToLoc)
import Swarm.Game.World.Syntax
import Swarm.Game.World.Typecheck

-- XXX note this doesn't depend at all on WExp etc., only imports
-- Syntax because of Over, Empty.  Should move those somewhere else?

------------------------------------------------------------
-- Bracket abstraction
------------------------------------------------------------

-- Explicitly type-preserving bracket abstraction, a la Oleg Kiselyov.
-- See:
--
--   http://okmij.org/ftp/tagless-final/ski.pdf
--   http://okmij.org/ftp/tagless-final/skconv.ml

--------------------------------------------------
-- Closed terms

-- | Closed, fully abstracted terms.  All computation is represented
--   by combinators.  This is the target for the bracket abstraction
--   operation.
data BTerm :: Type -> Type where
  BApp :: BTerm (a -> b) -> BTerm a -> BTerm b
  BConst :: Const a -> BTerm a

deriving instance Show (BTerm t)

instance Applicable BTerm where
  ($$) = BApp

instance HasConst BTerm where
  embed = BConst

--------------------------------------------------
-- Open terms

-- These explicitly open terms are an intermediate stage in the
-- bracket abstraction algorithm.
data OTerm :: [Type] -> Type -> Type where
  -- Embedded closed term.
  E :: BTerm a -> OTerm g a
  -- Reference to the innermost/top environment variable, i.e. Z
  V :: OTerm (a ': g) a
  -- Internalize the topmost env variable as a function argument
  N :: OTerm g (a -> b) -> OTerm (a ': g) b
  -- Ignore the topmost env variable
  W :: OTerm g b -> OTerm (a ': g) b

instance HasConst (OTerm g) where
  embed = E . embed

-- Bracket abstraction: convert the TTerm to an OTerm, then project
-- out the embedded BTerm.  GHC can see this is total since E is the
-- only constructor that can produce an OTerm with an empty
-- environment.
bracket :: TTerm '[] a -> BTerm a
bracket t = case conv t of
  E t' -> t'

-- Type-preserving conversion from TTerm to OTerm (conv + the
-- Applicable instance).  Taken directly from Kiselyov.
conv :: TTerm g a -> OTerm g a
conv (TVar VZ) = V
conv (TVar (VS x)) = W (conv (TVar x))
conv (TLam t) = case conv t of
  V -> E (BConst I)
  E d -> E (K .$$ d)
  N e -> e
  W e -> K .$$ e
conv (TApp t1 t2) = conv t1 $$ conv t2
conv (TConst c) = embed c

instance Applicable (OTerm g) where
  W e1 $$ W e2 = W (e1 $$ e2)
  W e $$ E d = W (e $$ E d)
  E d $$ W e = W (E d $$ e)
  W e $$ V = N e
  V $$ W e = N (E (C .$$. I) $$ e)
  W e1 $$ N e2 = N (B .$$ e1 $$ e2)
  N e1 $$ W e2 = N (C .$$ e1 $$ e2)
  N e1 $$ N e2 = N (S .$$ e1 $$ e2)
  N e $$ V = N (S .$$ e $$. I)
  V $$ N e = N (E (S .$$. I) $$ e)
  E d $$ N e = N (E (B .$$ d) $$ e)
  E d $$ V = N (E d)
  V $$ E d = N (E (C .$$. I $$ d))
  N e $$ E d = N (E (C .$$. C $$ d) $$ e)
  E d1 $$ E d2 = E (d1 $$ d2)

-- GHC can tell that V $$ V is impossible (it would be ill-typed)

------------------------------------------------------------
-- Compiling

-- Compiling to the host language. i.e. we co-opt the host language
-- into doing evaluation for us.

-- XXX more efficient than directly interpreting BTerms?  Should try
-- benchmarking.

data CTerm a where
  CFun :: (CTerm a -> CTerm b) -> CTerm (a -> b)
  CConst :: a -> CTerm a -- only for non-functions!

instance Applicable CTerm where
  CFun f $$ x = f x
  _ $$ _ = error "impossible! bad call to CTerm.$$, CConst should never contain functions"

compile :: BTerm a -> CTerm a
compile (BApp b1 b2) = compile b1 $$ compile b2
compile (BConst c) = compileConst c

compileConst :: Const a -> CTerm a
compileConst = \case
  (CLit i) -> CConst i
  CFI -> unary fromIntegral
  CIf -> CFun $ \(CConst b) -> CFun $ \t -> CFun $ \e -> if b then t else e
  CNot -> unary not
  CNeg -> unary negate
  CAnd -> binary (&&)
  COr -> binary (||)
  CAdd -> binary (+)
  CSub -> binary (-)
  CMul -> binary (*)
  CDiv -> binary (/)
  CIDiv -> binary div
  CMod -> binary mod
  CEq -> binary (==)
  CNeq -> binary (/=)
  CLt -> binary (<)
  CLeq -> binary (<=)
  CGt -> binary (>)
  CGeq -> binary (>=)
  CMask -> undefined
  CSeed -> undefined
  CCoord ax -> CFun $ \(CConst (coordsToLoc -> Location x y)) -> CConst (fromIntegral (case ax of X -> x; Y -> y))
  CHash -> undefined
  CPerlin -> undefined
  CReflect _r -> undefined
  CRot _r -> undefined
  COver -> binary (<+>)
  CEmpty -> CConst empty
  K -> CFun $ \x -> CFun $ const x
  S -> CFun $ \f -> CFun $ \g -> CFun $ \x -> f $$ x $$ (g $$ x)
  I -> CFun id
  B -> CFun $ \f -> CFun $ \g -> CFun $ \x -> f $$ (g $$ x)
  C -> CFun $ \f -> CFun $ \x -> CFun $ \y -> f $$ y $$ x
  Φ -> CFun $ \c -> CFun $ \f -> CFun $ \g -> CFun $ \x -> c $$ (f $$ x) $$ (g $$ x)

unary :: (a -> b) -> CTerm (a -> b)
unary op = CFun $ \(CConst x) -> CConst (op x)

binary :: (a -> b -> c) -> CTerm (a -> b -> c)
binary op = CFun $ \(CConst x) -> CFun $ \(CConst y) -> CConst (op x y)

runCTerm :: CTerm a -> a
runCTerm (CConst a) = a
runCTerm (CFun f) = runCTerm . f . CConst
