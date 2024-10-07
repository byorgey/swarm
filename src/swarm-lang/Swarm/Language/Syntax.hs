{-# LANGUAGE PatternSynonyms #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Abstract syntax for terms of the Swarm programming language.
module Swarm.Language.Syntax (
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
  ImportDir,
  PathStatus (..),
  ImportLoc (..),
  Syntax' (..),
  sLoc,
  sTerm,
  sType,
  sComments,
  Syntax,
  TSyntax,
  USyntax,
  pattern Syntax,
  pattern CSyntax,
  LocVar (..),
  LetSyntax (..),
  pattern STerm,
  pattern TRequirements,
  pattern TPair,
  pattern TLam,
  pattern TApp,
  pattern (:$:),
  pattern TLet,
  pattern TTydef,
  pattern TBind,
  pattern TDelay,
  pattern TRcd,
  pattern TProj,
  pattern TAnnotate,
  pattern TSuspend,
  pattern TImportIn,

  -- * Terms
  Var,
  Term' (..),
  Term,
  TTerm,
  UTerm,
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
import Swarm.Language.Syntax.Import
import Swarm.Language.Syntax.Loc
import Swarm.Language.Syntax.Pattern
import Swarm.Language.Syntax.Pretty ()
import Swarm.Language.Syntax.Util
import Swarm.Language.Types
