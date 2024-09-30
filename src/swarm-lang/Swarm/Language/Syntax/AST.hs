{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types representing the surface syntax and terms for Swarm programming language.
module Swarm.Language.Syntax.AST (
  ImportLocation,
  Syntax' (..),
  LetSyntax (..),
  Term' (..),
) where

import Control.Lens (Plated (..))
import Data.Aeson.Types hiding (Key)
import Data.Data (Data)
import Data.Data.Lens (uniplate)
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Swarm.Language.Requirements.Type (Requirements)
import Swarm.Language.Syntax.Comments
import Swarm.Language.Syntax.Constants
import Swarm.Language.Syntax.Direction
import Swarm.Language.Syntax.Loc
import Swarm.Language.Types

------------------------------------------------------------
-- Syntax: annotation on top of Terms with SrcLoc, comments, + type
------------------------------------------------------------

-- | The surface syntax for the language, with location and type annotations.
data Syntax' ty = Syntax'
  { _sLoc :: SrcLoc
  , _sTerm :: Term' ty
  , _sComments :: Comments
  , _sType :: ty
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Data, Generic, Hashable)

instance Data ty => Plated (Syntax' ty) where
  plate = uniplate

-- | A @let@ expression can be written either as @let x = e1 in e2@ or
--   as @def x = e1 end; e2@. This enumeration simply records which it
--   was so that we can pretty-print appropriatly.
data LetSyntax = LSLet | LSDef
  deriving (Eq, Ord, Show, Bounded, Enum, Generic, Data, Hashable, ToJSON, FromJSON)

------------------------------------------------------------
-- Term: basic syntax tree
------------------------------------------------------------

-- | Terms of the Swarm language.
data Term' ty
  = -- | The unit value.
    TUnit
  | -- | A constant.
    TConst Const
  | -- | A direction literal.
    TDir Direction
  | -- | An integer literal.
    TInt Integer
  | -- | An antiquoted Haskell variable name of type Integer.
    TAntiInt Text
  | -- | A text literal.
    TText Text
  | -- | An antiquoted Haskell variable name of type Text.
    TAntiText Text
  | -- | A Boolean literal.
    TBool Bool
  | -- | A robot reference.  These never show up in surface syntax, but are
    --   here so we can factor pretty-printing for Values through
    --   pretty-printing for Terms.
    TRobot Int
  | -- | A memory reference.  These likewise never show up in surface syntax,
    --   but are here to facilitate pretty-printing.
    TRef Int
  | -- | Require a specific device to be installed.
    TRequireDevice Text
  | -- | Require a certain number of an entity.
    TRequire Int Text
  | -- | Primitive command to log requirements of a term.  The Text
    --   field is to store the unaltered original text of the term, for use
    --   in displaying the log message (since once we get to execution time the
    --   original term may have been elaborated, e.g. `force` may have been added
    --   around some variables, etc.)
    SRequirements Text (Syntax' ty)
  | -- | A variable.
    TVar Var
  | -- | A pair.
    SPair (Syntax' ty) (Syntax' ty)
  | -- | A lambda expression, with or without a type annotation on the
    --   binder.
    SLam LocVar (Maybe Type) (Syntax' ty)
  | -- | Function application.
    SApp (Syntax' ty) (Syntax' ty)
  | -- | A (recursive) let/def expression, with or without a type
    --   annotation on the variable. The @Bool@ indicates whether
    --   it is known to be recursive.
    --
    --   The @Maybe Polytype@ and @Maybe Requirements@ fields are only
    --   for annotating the requirements of a definition after
    --   typechecking; there is no way to annotate requirements in the
    --   surface syntax.
    SLet LetSyntax Bool LocVar (Maybe RawPolytype) (Maybe Polytype) (Maybe Requirements) (Syntax' ty) (Syntax' ty)
  | -- | A type synonym definition.  Note that this acts like a @let@
    --   (just like @def@), /i.e./ the @Syntax' ty@ field is the local
    --   context over which the type definition is in scope.
    STydef LocVar Polytype (Maybe TydefInfo) (Syntax' ty)
  | -- | A monadic bind for commands, of the form @c1 ; c2@ or @x <- c1; c2@.
    --
    --   The @Maybe ty@ field is a place to stash the inferred type of
    --   the variable (if any) during type inference.  Once type
    --   inference is complete, during elaboration we will copy the
    --   inferred type into the @Maybe Polytype@ field (since the
    --   @Maybe ty@ field will be erased).
    --
    --   The @Maybe Polytype@ and @Maybe Requirements@ fields is only
    --   for annotating the type of a bind after typechecking; there
    --   is no surface syntax that allows directly annotating a bind
    --   with either one.
    SBind (Maybe LocVar) (Maybe ty) (Maybe Polytype) (Maybe Requirements) (Syntax' ty) (Syntax' ty)
  | -- | Delay evaluation of a term, written @{...}@.  Swarm is an
    --   eager language, but in some cases (e.g. for @if@ statements
    --   and recursive bindings) we need to delay evaluation.  The
    --   counterpart to @{...}@ is @force@, where @force {t} = t@.
    --   Note that 'Force' is just a constant, whereas 'SDelay' has to
    --   be a special syntactic form so its argument can get special
    --   treatment during evaluation.
    SDelay (Syntax' ty)
  | -- | Record literals @[x1 = e1, x2 = e2, x3, ...]@ Names @x@
    --   without an accompanying definition are sugar for writing
    --   @x=x@.
    SRcd (Map Var (Maybe (Syntax' ty)))
  | -- | Record projection @e.x@
    SProj (Syntax' ty) Var
  | -- | Annotate a term with a type
    SAnnotate (Syntax' ty) RawPolytype
  | -- | Run the given command, then suspend and wait for a new REPL
    --   input.
    SSuspend (Syntax' ty)
  | -- | A type literal.
    TType Type
  | -- | Import a term containing definitions, which will be in scope
    --   in the following term.
    SImportIn ImportLocation (Syntax' ty)
  deriving
    ( Eq
    , Show
    , Functor
    , Foldable
    , Data
    , Generic
    , Hashable
    , -- | The Traversable instance for Term (and for Syntax') is used during
      -- typechecking: during intermediate type inference, many of the type
      -- annotations placed on AST nodes will have unification variables in
      -- them. Once we have finished solving everything we need to do a
      -- final traversal over all the types in the AST to substitute away
      -- all the unification variables (and generalize, i.e. stick 'forall'
      -- on, as appropriate).  See the call to 'mapM' in
      -- Swarm.Language.Typecheck.runInfer.
      Traversable
    )

instance Data ty => Plated (Term' ty) where
  plate = uniplate

-- | A location from which to import a `.sw` file.
type ImportLocation = FilePath

-- Note, it could make sense to use a more structured type for the
-- import location, e.g. something like
--
--   data ImportLocation = LocalFile Text | RemoteFile URL
--
-- (dhall does something like this; see
-- https://hackage.haskell.org/package/dhall-1.42.1/docs/Dhall-Core.html#t:ImportType). For
-- now, we leave things simple.
