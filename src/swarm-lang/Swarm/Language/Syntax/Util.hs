{-# LANGUAGE PatternSynonyms #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Helper functions for working with @Terms@ and @Syntax@
module Swarm.Language.Syntax.Util (
  mkOp,
  mkOp',
  unfoldApps,
  mkTuple,
  unTuple,

  -- * Erasure
  erase,
  eraseS,

  -- * Term traversal
  freeVarsS,
  freeVarsT,
  freeVarsV,
  mapFreeS,
  locVarToSyntax',
  asTree,
  measureAstSize,
) where

import Control.Lens (Traversal', para, universe, (%~), (^.), pattern Empty)
import Control.Monad (void)
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as S
import Data.Tree
import Swarm.Language.Context (Var)
import Swarm.Language.Syntax.AST
import Swarm.Language.Syntax.Constants
import Swarm.Language.Syntax.Loc
import Swarm.Language.Syntax.Pattern

-- Setup for doctests

-- $setup
-- >>> import Control.Lens ((^.))
-- >>> import Swarm.Language.Syntax.Constants
-- >>> import Swarm.Language.Syntax.Loc
-- >>> import Swarm.Language.Syntax.Pattern
-- >>> import Swarm.Language.Syntax.AST

-- | Make an infix operation (e.g. @2 + 3@) a curried function
--   application (e.g. @((+) 2) 3@).
mkOp :: Const -> Syntax -> Syntax -> Syntax
mkOp c s1@(Syntax l1 _) s2@(Syntax l2 _) = Syntax newLoc newTerm
 where
  -- The new syntax span both terms
  newLoc = l1 <> l2
  -- We don't assign a source location for the operator since it is
  -- usually provided as-is and it is not likely to be useful.
  sop = noLoc (TConst c)
  newTerm = SApp (Syntax l1 $ SApp sop s1) s2

-- | Make an infix operation, discarding any location information
mkOp' :: Const -> Term -> Term -> Term
mkOp' c t1 = TApp (TApp (TConst c) t1)

-- | Turn function application chain into a list.
--
-- >>> syntaxWrap f = fmap (^. sTerm) . f . Syntax NoLoc
-- >>> syntaxWrap unfoldApps (mkOp' Mul (TInt 1) (TInt 2)) -- 1 * 2
-- TConst Mul :| [TInt 1,TInt 2]
unfoldApps :: Syntax' ty -> NonEmpty (Syntax' ty)
unfoldApps trm = NonEmpty.reverse . flip NonEmpty.unfoldr trm $ \case
  Syntax' _ (SApp s1 s2) _ _ -> (s2, Just s1)
  s -> (s, Nothing)

-- | Create a nested tuple out of a list of syntax nodes.
mkTuple :: [Syntax] -> Syntax
mkTuple [] = Syntax NoLoc TUnit -- should never happen
mkTuple [x] = x
mkTuple (x : xs) = let r = mkTuple xs in loc x r $ SPair x r
 where
  loc a b = Syntax $ (a ^. sLoc) <> (b ^. sLoc)

-- | Decompose a nested tuple into a list of components.
unTuple :: Syntax' ty -> [Syntax' ty]
unTuple = \case
  Syntax' _ (SPair s1 s2) _ _ -> s1 : unTuple s2
  s -> [s]

------------------------------------------------------------
-- Type erasure
------------------------------------------------------------

-- | Erase the type annotations from a 'Syntax' or 'Term' tree.
erase :: Functor t => t ty -> t ()
erase = void

-- | Erase all annotations from a 'Syntax' node, turning it into a
--   bare 'Term'.
eraseS :: Syntax' ty -> Term
eraseS (Syntax' _ t _ _) = erase t

------------------------------------------------------------
-- Free variable traversals
------------------------------------------------------------

-- | Traversal over those subterms of a term which represent free
--   variables.  The S suffix indicates that it is a `Traversal' over
--   the `Syntax` nodes (which contain type and source location info)
--   containing free variables inside a larger `Syntax` value.  Note
--   that if you want to get the list of all `Syntax` nodes
--   representing free variables, you can do so via @'toListOf'
--   'freeVarsS'@.
freeVarsS :: forall ty. Traversal' (Syntax' ty) (Syntax' ty)
freeVarsS f = go S.empty
 where
  -- go :: Applicative f => Set Var -> Syntax' ty -> f (Syntax' ty)
  go bound s@(Syntax' l t ty cmts) = case t of
    TUnit -> pure s
    TConst {} -> pure s
    TDir {} -> pure s
    TInt {} -> pure s
    TAntiInt {} -> pure s
    TText {} -> pure s
    TAntiText {} -> pure s
    TBool {} -> pure s
    TRobot {} -> pure s
    TRef {} -> pure s
    TRequireDevice {} -> pure s
    TRequire {} -> pure s
    SRequirements x s1 -> rewrap $ SRequirements x <$> go bound s1
    TVar x
      | x `S.member` bound -> pure s
      | otherwise -> f s
    SLam x xty s1 -> rewrap $ SLam x xty <$> go (S.insert (lvVar x) bound) s1
    SApp s1 s2 -> rewrap $ SApp <$> go bound s1 <*> go bound s2
    SLet ls r x xty xpty xreq s1 s2 ->
      let bound' = S.insert (lvVar x) bound
       in rewrap $ SLet ls r x xty xpty xreq <$> go bound' s1 <*> go bound' s2
    STydef x xdef tdInfo t1 -> rewrap $ STydef x xdef tdInfo <$> go bound t1
    SPair s1 s2 -> rewrap $ SPair <$> go bound s1 <*> go bound s2
    SBind mx mty mpty mreq s1 s2 -> rewrap $ SBind mx mty mpty mreq <$> go bound s1 <*> go (maybe id (S.insert . lvVar) mx bound) s2
    SDelay s1 -> rewrap $ SDelay <$> go bound s1
    SRcd m -> rewrap $ SRcd <$> (traverse . traverse) (go bound) m
    SProj s1 x -> rewrap $ SProj <$> go bound s1 <*> pure x
    SAnnotate s1 pty -> rewrap $ SAnnotate <$> go bound s1 <*> pure pty
    SSuspend s1 -> rewrap $ SSuspend <$> go bound s1
    TType {} -> pure s
    SImportIn url s1 -> rewrap $ SImportIn url <$> go bound s1
   where
    rewrap s' = Syntax' l <$> s' <*> pure ty <*> pure cmts

-- | Like 'freeVarsS', but traverse over the 'Term's containing free
--   variables.  More direct if you don't need to know the types or
--   source locations of the variables.  Note that if you want to get
--   the list of all `Term`s representing free variables, you can do
--   so via @'toListOf' 'freeVarsT'@.
freeVarsT :: forall ty. Traversal' (Syntax' ty) (Term' ty)
freeVarsT = freeVarsS . sTerm

-- | Traversal over the free variables of a term.  Like 'freeVarsS'
--   and 'freeVarsT', but traverse over the variable names themselves.
--   Note that if you want to get the set of all free variable names,
--   you can do so via @'Data.Set.Lens.setOf' 'freeVarsV'@.
freeVarsV :: Traversal' (Syntax' ty) Var
freeVarsV = freeVarsT . (\f -> \case TVar x -> TVar <$> f x; t -> pure t)

-- | Apply a function to all free occurrences of a particular
--   variable.
mapFreeS :: Var -> (Syntax' ty -> Syntax' ty) -> Syntax' ty -> Syntax' ty
mapFreeS x f = freeVarsS %~ (\t -> case t ^. sTerm of TVar y | y == x -> f t; _ -> t)

-- | Transform the AST into a Tree datatype.  Useful for
--   pretty-printing (e.g. via "Data.Tree.drawTree").
asTree :: Data a => Syntax' a -> Tree (Syntax' a)
asTree = para Node

-- | Each constructor is a assigned a value of 1, plus
--   any recursive syntax it entails.
measureAstSize :: Data a => Syntax' a -> Int
measureAstSize = length . filter (not . isNoop) . universe

-- | Don't count "noop" nodes towards the code size.  They are usually
--   inserted automatically, either in @{}@ or after a bare @def@.
isNoop :: Syntax' a -> Bool
isNoop = \case
  Syntax' _ (TConst Noop) _ _ -> True
  _ -> False

locVarToSyntax' :: LocVar -> ty -> Syntax' ty
locVarToSyntax' (LV s v) = Syntax' s (TVar v) Empty
