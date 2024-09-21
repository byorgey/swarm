-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Analyze a term to discover the requirements for
-- evaluating/executing it.
--
-- Note, eventually, requirements will be part of types and
-- requirements analysis will just be part of typechecking
-- (https://github.com/swarm-game/swarm/issues/231).
module Swarm.Language.Requirements.Analysis (
  requirements,
) where

import Control.Algebra (Has, run)
import Control.Carrier.Accum.Strict (execAccum)
import Control.Carrier.Reader (runReader)
import Control.Effect.Accum (Accum, add)
import Control.Effect.Reader (Reader, ask, local)
import Control.Monad (when)
import Data.Fix (Fix (..))
import Data.Foldable (forM_)
import Data.Map qualified as M
import Swarm.Language.Capability (Capability (..), constCaps)
import Swarm.Language.Context qualified as Ctx
import Swarm.Language.Requirements.Type
import Swarm.Language.Syntax
import Swarm.Language.Syntax.Direction (isCardinal)
import Swarm.Language.Types
import Swarm.Util (applyWhen)

-- | Infer the requirements to execute/evaluate a term in a given
--   context.
--
--   For function application and let-expressions, we assume that the
--   argument (respectively let-bound expression) is used at least
--   once in the body.  Doing otherwise would require a much more
--   fine-grained analysis where we differentiate between the
--   capabilities needed to *evaluate* versus *execute* any expression
--   (since e.g. an unused let-binding would still incur the
--   capabilities to *evaluate* it), which does not seem worth it at
--   all.
--
--   This is all a bit of a hack at the moment, to be honest; see #231
--   for a description of a more correct approach.
requirements :: TDCtx -> ReqCtx -> Term -> Requirements
requirements tdCtx ctx =
  run . execAccum mempty . runReader tdCtx . runReader ctx . (add (singletonCap CPower) *>) . go
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
    TSuspend {} -> pure ()
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
    -- that's OK; if not, it just means using the variable requires
    -- no special capabilities.
    TVar x -> do
      reqs <- ask @ReqCtx
      forM_ (Ctx.lookup x reqs) add
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
      mapM_ typeRequirements mty
      local @ReqCtx (Ctx.delete x) $ go t
    -- Special case for 'use' with a device literal.
    TApp t1@(TConst Use) t2@(TText device) ->
      add (singletonDev device) *> go t1 *> go t2
    -- In general, an application simply requires the union of the
    -- capabilities from the left- and right-hand sides.  This assumes
    -- that the argument will be used at least once by the function.
    TApp t1 t2 -> go t1 *> go t2
    -- Similarly, for a let, we assume that the let-bound expression
    -- will be used at least once in the body. We delete the let-bound
    -- name from the context when recursing for the same reason as
    -- lambda.
    TLet LSLet r x mty _ _ t1 t2 -> do
      when r $ add (singletonCap CRecursion)
      add (singletonCap CEnv)
      mapM_ polytypeRequirements mty
      local @ReqCtx (Ctx.delete x) $ go t1 *> go t2
    -- However, for def, we do NOT assume that the defined expression
    -- will be used at least once in the body; it may not be executed
    -- until later on, when the base robot has more capabilities.
    TLet LSDef r x mty _ _ t1 t2 -> do
      add (singletonCap CEnv)
      mapM_ polytypeRequirements mty
      localReqCtx <- ask @ReqCtx
      localTDCtx <- ask @TDCtx
      let bodyReqs =
            applyWhen r (singletonCap CRecursion <>) $
              requirements localTDCtx localReqCtx t1
      local @ReqCtx (Ctx.addBinding x bodyReqs) $ go t2
    -- Using tydef requires CEnv, plus whatever the requirements are
    -- for the type itself.
    TTydef x ty _ t2 -> do
      add (singletonCap CEnv)
      polytypeRequirements ty
      -- Now check the nested term with the new type definition added
      -- to the context.
      --
      -- Note, it's not ideal to be creating a TydefInfo from scratch
      -- here; ideally we should get it from the kind checker.
      -- Eventually we will put that into the third field of TTydef,
      -- but it's not there yet at this point.  This is really just a
      -- symptom of the fact that typechecking, kind checking, and
      -- requirements checking really all need to be done at the same
      -- time during a single traversal of the term (see #231).
      local @TDCtx (Ctx.addBinding x (TydefInfo ty (Arity . length . ptVars $ ty))) (go t2)
    -- We also delete the name in a TBind, if any, while recursing on
    -- the RHS.
    TBind mx _ _ t1 t2 -> do
      go t1
      local @ReqCtx (maybe id Ctx.delete mx) $ go t2
    -- Everything else is straightforward.
    TPair t1 t2 -> add (singletonCap CProd) *> go t1 *> go t2
    TDelay t -> go t
    TRcd m -> add (singletonCap CRecord) *> forM_ (M.assocs m) (go . expandEq)
     where
      expandEq (x, Nothing) = TVar x
      expandEq (_, Just t) = t
    TProj t _ -> add (singletonCap CRecord) *> go t
    -- XXX Should import require a dictionary or something like that?
    TImportIn _loc t -> go t
    -- A type ascription doesn't change requirements
    TAnnotate t ty -> go t *> polytypeRequirements ty

polytypeRequirements ::
  (Has (Accum Requirements) sig m, Has (Reader TDCtx) sig m) =>
  Poly q Type ->
  m ()
polytypeRequirements = typeRequirements . ptBody

typeRequirements ::
  (Has (Accum Requirements) sig m, Has (Reader TDCtx) sig m) =>
  Type ->
  m ()
typeRequirements = go
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
