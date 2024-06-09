-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A requirement is something that is needed in order to successfully
-- build a robot running a certain program.
module Swarm.Language.Requirement (
  -- * Requirements

  -- ** The 'Requirement' type
  Requirement (..),

  -- ** The 'Requirements' type and utility functions
  Requirements (..),
  singleton,
  singletonCap,
  singletonDev,
  singletonInv,
  insert,
  ReqCtx,

  -- * Requirements analysis
  requirements,
) where

import Control.Algebra (Has, run)
import Control.Carrier.Accum.Strict (execAccum)
import Control.Carrier.Reader (runReader)
import Control.Effect.Accum (Accum, add)
import Control.Effect.Reader (Reader, local)
import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first)
import Data.Data (Data)
import Data.Fix (Fix (..))
import Data.Foldable (forM_)
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import GHC.Generics (Generic)
import Swarm.Language.Capability (Capability (..), constCaps)
import Swarm.Language.Context (Ctx)
import Swarm.Language.Context qualified as Ctx
import Swarm.Language.Syntax
import Swarm.Language.Syntax.Direction (isCardinal)
import Swarm.Language.Types

-- | A /requirement/ is something a robot must have when it is
--   built. There are three types:
--   - A robot can require a certain 'Capability', which should be fulfilled
--     by equipping an appropriate device.
--   - A robot can require a specific /device/, which should be equipped.
--   - A robot can require some number of a specific entity in its inventory.
data Requirement
  = -- | Require a specific capability.  This must be fulfilled by
    --   equipping an appropriate device.  Requiring the same
    --   capability multiple times is the same as requiring it once.
    ReqCap Capability
  | -- | Require a specific device to be equipped.  Note that at this
    --   point it is only a name, and has not been resolved to an actual
    --   'Swarm.Game.Entity.Entity'.  That's because programs have to be type- and
    --   capability-checked independent of an 'Swarm.Game.Entity.EntityMap'.  The name
    --   will be looked up at runtime, when actually executing a 'Swarm.Language.Syntax.Build'
    --   or 'Swarm.Language.Syntax.Reprogram' command, and an appropriate exception thrown if
    --   a device with the given name does not exist.
    --
    --   Requiring the same device multiple times is the same as
    --   requiring it once.
    ReqDev Text
  | -- | Require a certain number of a specific entity to be available
    --   in the inventory.  The same comments apply re: resolving the
    --   entity name to an actual 'Swarm.Game.Entity.Entity'.
    --
    --   Inventory requirements are additive, that is, say, requiring 5
    --   of entity @"e"@ and later requiring 7 is the same as requiring
    --   12.
    ReqInv Int Text
  deriving (Eq, Ord, Show, Read, Generic, Hashable, Data, FromJSON, ToJSON)

-- | It is tempting to define @Requirements = Set Requirement@, but
--   that would be wrong, since two identical 'ReqInv' should have
--   their counts added rather than simply being deduplicated.
--
--   Since we will eventually need to deal with the different types of
--   requirements separately, it makes sense to store them separately
--   anyway.
data Requirements = Requirements
  { capReqs :: Set Capability
  , devReqs :: Set Text
  , invReqs :: Map Text Int
  }
  deriving (Eq, Ord, Show, Data, Generic, FromJSON, ToJSON)

instance Semigroup Requirements where
  Requirements c1 d1 i1 <> Requirements c2 d2 i2 =
    Requirements (c1 <> c2) (d1 <> d2) (M.unionWith (+) i1 i2)

instance Monoid Requirements where
  mempty = Requirements S.empty S.empty M.empty

-- | Create a 'Requirements' set with a single 'Requirement'.
singleton :: Requirement -> Requirements
singleton (ReqCap c) = Requirements (S.singleton c) S.empty M.empty
singleton (ReqDev d) = Requirements S.empty (S.singleton d) M.empty
singleton (ReqInv n e) = Requirements S.empty S.empty (M.singleton e n)

-- | For convenience, create a 'Requirements' set with a single
--   'Capability' requirement.
singletonCap :: Capability -> Requirements
singletonCap = singleton . ReqCap

-- | For convenience, create a 'Requirements' set with a single
--   device requirement.
singletonDev :: Text -> Requirements
singletonDev = singleton . ReqDev

-- | For convenience, create a 'Requirements' set with a single
--   inventory requirement.
singletonInv :: Int -> Text -> Requirements
singletonInv n e = singleton (ReqInv n e)

insert :: Requirement -> Requirements -> Requirements
insert = (<>) . singleton

-- | A requirement context records the requirements for the
--   definitions bound to variables.
type ReqCtx = Ctx Requirements

-- | Analyze a program to see what the requirements are to execute
--   it. Also return a context mapping from any variables declared via
--   'TDef' to the requirements for their definitions.
--
--   Note that this is necessarily a conservative analysis, especially
--   if the program contains conditional expressions.  Some
--   capabilities may end up not being actually needed if certain
--   commands end up not being executed.  However, the analysis should
--   be safe in the sense that a robot with the indicated capabilities
--   will always be able to run the given program.
requirements :: TDCtx -> ReqCtx -> Term -> (Requirements, ReqCtx)
requirements tdCtx ctx tm = first (insert (ReqCap CPower)) $ case tm of
  -- First, at the top level, we have to keep track of the
  -- requirements for variables bound with the 'TDef' command.

  -- To make a definition requires the env capability.  Note that the
  -- act of MAKING the definition does not require the capabilities of
  -- the body of the definition (including the possibility of the
  -- recursion capability, if the definition is recursive).  However,
  -- we also return a map which associates the defined name to the
  -- capabilities it requires.
  TDef r x mty t ->
    let tyReqs = maybe mempty (polytypeRequirements tdCtx) mty
        bodyReqs =
          (if r then insert (ReqCap CRecursion) else id)
            (termRequirements tdCtx ctx t)
     in (singletonCap CEnv <> tyReqs, Ctx.singleton x bodyReqs)
  -- Making a type synonym also requires CEnv.
  TTydef _ ty -> (singletonCap CEnv <> polytypeRequirements tdCtx ty, Ctx.empty)
  TBind _ t1 t2 ->
    -- First, see what the requirements are to execute the
    -- first command.  It may also define some names, so we get a
    -- map of those names to their required capabilities.
    let (reqs1, ctx1) = requirements tdCtx ctx t1

        -- Now see what capabilities are required for the second
        -- command; use an extended context since it may refer to
        -- things defined in the first command.
        ctx' = ctx `Ctx.union` ctx1
        (reqs2, ctx2) = requirements tdCtx ctx' t2
     in -- Finally return the union of everything.
        (reqs1 <> reqs2, ctx' `Ctx.union` ctx2)
  -- Any other term can't bind variables with 'TDef', so we no longer
  -- need to worry about tracking a returned context.
  _ -> (termRequirements tdCtx ctx tm, Ctx.empty)

-- In theory we could rewrite the requirements function above to use
-- effects/capability style, like the ones below, but trying to figure
-- out the right effects to use and how to manage the flow of
-- information hurts my brain.  So I think it's simpler to just leave
-- it implemented in a direct style.

-- | Infer the requirements to execute/evaluate a term in a
--   given context, where the term is guaranteed not to contain any
--   'TDef'.
--
--   For function application and let-expressions, we assume that the
--   argument (respectively let-bound expression) is used at least
--   once in the body.  Doing otherwise would require a much more
--   fine-grained analysis where we differentiate between the
--   capabilities needed to *evaluate* versus *execute* any expression
--   (since e.g. an unused let-binding would still incur the
--   capabilities to *evaluate* it), which does not seem worth it at
--   all.
termRequirements :: TDCtx -> ReqCtx -> Term -> Requirements
termRequirements tdCtx ctx = run . execAccum mempty . runReader tdCtx . runReader ctx . go
 where
  go ::
    ( Has (Accum Requirements) sig m
    , Has (Reader ReqCtx) sig m
    , Has (Reader TDCtx) sig m
    ) =>
    Term ->
    m ()
  go = \case
    -- Some primitive literals that don't require any special
    -- capability.
    TUnit -> pure ()
    TDir d -> when (isCardinal d) $ add (singletonCap COrient)
    TInt _ -> pure ()
    TAntiInt _ -> pure ()
    TText _ -> pure ()
    TAntiText _ -> pure ()
    TBool _ -> pure ()
    -- It doesn't require any special capability to *inquire* about
    -- the requirements of a term.
    TRequirements _ _ -> pure ()
    -- Look up the capabilities required by a function/command
    -- constants using 'constCaps'.
    TConst c -> forM_ (constCaps c) (add . singletonCap)
    -- Simply record device or inventory requirements.
    TRequireDevice d -> add (singletonDev d)
    TRequire n e -> add (singletonInv n e)
    -- Note that a variable might not show up in the context, and
    -- that's OK.  In particular, only variables bound by 'TDef' go
    -- in the context; variables bound by a lambda or let will not
    -- be there.
    TVar x -> forM_ (Ctx.lookup x ctx) add
    -- A lambda expression requires the 'CLambda' capability, and
    -- also all the capabilities of the body.  We assume that the
    -- lambda will eventually get applied, at which point it will
    -- indeed require the body's capabilities (this is unnecessarily
    -- conservative if the lambda is never applied, but such a
    -- program could easily be rewritten without the unused
    -- lambda). We also don't do anything with the argument: we
    -- assume that it is used at least once within the body, and the
    -- capabilities required by any argument will be picked up at
    -- the application site.  Again, this is overly conservative in
    -- the case that the argument is unused, but in that case the
    -- unused argument could be removed.
    --
    -- Note, however, that we do need to *delete* the argument from
    -- the context, in case the context already contains a definition
    -- with the same name: inside the lambda that definition will be
    -- shadowed, so we do not want the name to be associated to any
    -- capabilities.
    TLam x mty t -> do
      add (singletonCap CLambda)
      mapM_ typeRequirements' mty
      local @ReqCtx (Ctx.delete x) $ go t
    -- An application simply requires the union of the capabilities
    -- from the left- and right-hand sides.  This assumes that the
    -- argument will be used at least once by the function.
    TApp t1 t2 -> go t1 *> go t2
    -- Similarly, for a let, we assume that the let-bound expression
    -- will be used at least once in the body. We delete the let-bound
    -- name from the context when recursing for the same reason as
    -- lambda.
    TLet r x mty t1 t2 -> do
      when r $ add (singletonCap CRecursion)
      add (singletonCap CEnv)
      mapM_ polytypeRequirements' mty
      local @ReqCtx (Ctx.delete x) $ go t1 *> go t2
    -- We also delete the name in a TBind, if any, while recursing on
    -- the RHS.
    TBind mx t1 t2 -> do
      go t1
      local @ReqCtx (maybe id Ctx.delete mx) $ go t2
    -- Everything else is straightforward.
    TPair t1 t2 -> add (singletonCap CProd) *> go t1 *> go t2
    TDelay _ t -> go t
    TRcd m -> add (singletonCap CRecord) *> forM_ (M.assocs m) (go . expandEq)
     where
      expandEq (x, Nothing) = TVar x
      expandEq (_, Just t) = t
    TProj t _ -> add (singletonCap CRecord) *> go t
    -- A type ascription doesn't change requirements
    TAnnotate t ty -> go t *> polytypeRequirements' ty
    -- These cases should never happen if the term has been
    -- typechecked; Def commands are only allowed at the top level.
    TDef {} -> pure ()
    TTydef {} -> pure ()

-- | Infer the requirements to mention a given type.
polytypeRequirements :: TDCtx -> Polytype -> Requirements
polytypeRequirements tdCtx = run . execAccum mempty . runReader tdCtx . polytypeRequirements'

polytypeRequirements' ::
  (Has (Accum Requirements) sig m, Has (Reader TDCtx) sig m) =>
  Polytype ->
  m ()
polytypeRequirements' (Forall _ ty) = typeRequirements' ty

typeRequirements' ::
  (Has (Accum Requirements) sig m, Has (Reader TDCtx) sig m) =>
  Type ->
  m ()
typeRequirements' = go
 where
  go (Fix tyF) = goF tyF

  goF = \case
    TyVarF _ -> pure ()
    TyConF (TCUser u) tys -> do
      mapM_ go tys
      ty' <- expandTydef u tys
      go ty'
    TyConF c tys -> do
      case c of
        TCSum -> add (singletonCap CSum)
        TCProd -> add (singletonCap CProd)
        _ -> pure ()
      mapM_ go tys
    TyRcdF m -> mapM_ go m
    TyRecF _ ty' -> add (singletonCap CRectype) *> go ty'
    TyRecVarF _ -> pure ()
