{-# LANGUAGE PatternSynonyms #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Abstract syntax for terms of the Swarm programming language.
module Swarm.Language.Syntax (
  -- * Directions
  Direction (..),
  AbsoluteDir (..),
  RelativeDir (..),
  PlanarRelativeDir (..),
  directionSyntax,
  isCardinal,
  allDirs,

  -- * Constants
  Const (..),
  allConst,
  ConstInfo (..),
  ConstDoc (..),
  ConstMeta (..),
  MBinAssoc (..),
  MUnAssoc (..),
  constInfo,
  arity,
  isCmd,
  isUserFunc,
  isOperator,
  isBuiltinFunction,
  isTangible,
  isLong,

  -- * Size limits
  maxSniffRange,
  maxScoutRange,
  maxStrideRange,
  maxPathRange,
  globalMaxVolume,

  -- * SrcLoc
  SrcLoc (..),
  srcLocBefore,
  noLoc,

  -- * Comments
  CommentType (..),
  CommentSituation (..),
  isStandalone,
  Comment (..),
  Comments (..),
  beforeComments,
  afterComments,

  -- * Syntax
  Syntax' (..),
  sLoc,
  sTerm,
  sType,
  sComments,
  Syntax,
  pattern Syntax,
  pattern CSyntax,
  LocVar (..),
  pattern STerm,
  pattern TRequirements,
  pattern TPair,
  pattern TLam,
  pattern TApp,
  pattern (:$:),
  pattern TLet,
  pattern TDef,
  pattern TBind,
  pattern TDelay,
  pattern TRcd,
  pattern TProj,
  pattern TAnnotate,

  -- * Terms
  Var,
  DelayType (..),
  Term' (..),
  Term,
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

import Swarm.Language.Syntax.AST
import Swarm.Language.Syntax.Comments
import Swarm.Language.Syntax.Constants
import Swarm.Language.Syntax.Direction
import Swarm.Language.Syntax.Loc
import Swarm.Language.Syntax.Pattern
import Swarm.Language.Syntax.Util
import Swarm.Language.Types
