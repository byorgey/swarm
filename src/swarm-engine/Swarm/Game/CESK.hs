{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: State machine of Swarm's interpreter
--
-- The Swarm interpreter uses a technique known as a
-- <https://matt.might.net/articles/cesk-machines/ CESK machine> (if
-- you want to read up on them, you may want to start by reading about
-- <https://matt.might.net/articles/cek-machines/ CEK machines>
-- first).  Execution happens simply by iterating a step function,
-- sending one state of the CESK machine to the next. In addition to
-- being relatively efficient, this means we can easily run a bunch of
-- robots synchronously, in parallel, without resorting to any threads
-- (by stepping their machines in a round-robin fashion); pause and
-- single-step the game; save and resume, and so on.
--
-- Essentially, a CESK machine state has four components:
--
-- - The __C__ontrol is the thing we are currently focused on:
--   either a 'Term' to evaluate, or a 'Value' that we have
--   just finished evaluating.
-- - The __E__nvironment ('Env') is a mapping from variables that might
--   occur free in the Control to their values.
-- - The __S__tore ('Store') is a mapping from abstract integer
--   /locations/ to values.  We use it to store delayed (lazy) values,
--   so they will be computed at most once.
-- - The __K__ontinuation ('Cont') is a stack of 'Frame's,
--   representing the evaluation context, /i.e./ what we are supposed
--   to do after we finish with the currently focused thing.  When we
--   reduce the currently focused term to a value, the top frame on
--   the stack tells us how to proceed.
--
-- You can think of a CESK machine as a defunctionalization of a
-- recursive big-step interpreter, where we explicitly keep track of
-- the call stack and the environments that would be in effect at
-- various places in the recursion.  One could probably even derive
-- this mechanically, by writing a recursive big-step interpreter,
-- then converting it to CPS, then defunctionalizing the
-- continuations.
--
-- The slightly confusing thing about CESK machines is how we
-- have to pass around environments everywhere.  Basically,
-- anywhere there can be unevaluated terms containing free
-- variables (in values, in continuation stack frames, ...), we
-- have to store the proper environment alongside so that when
-- we eventually get around to evaluating it, we will be able to
-- pull out the environment to use.
module Swarm.Game.CESK (
  -- * Frames and continuations
  Frame (..),
  Cont,

  -- ** Wrappers for creating delayed change of state
  WorldUpdate (..),
  RobotUpdate (..),

  -- * Store
  Store,
  Addr,
  emptyStore,
  MemCell (..),
  allocate,
  lookupStore,
  setStore,

  -- * CESK machine states
  CESK (..),

  -- ** Construction
  initEmptyMachine,
  initMachine,
  initMachine',
  continue,
  cancel,
  resetBlackholes,

  -- ** Extracting information
  finalValue,
  suspendedEnv,
) where

import Control.Lens (Traversal', traversal, (^.))
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import GHC.Generics (Generic)
import Prettyprinter (Doc, Pretty (..), encloseSep, hsep, (<+>))
import Swarm.Game.Entity (Entity)
import Swarm.Game.Exception
import Swarm.Game.Ingredients (Count)
import Swarm.Game.Tick
import Swarm.Game.World (WorldUpdate (..))
import Swarm.Language.Context
import Swarm.Language.Pretty
import Swarm.Language.Requirements.Type (Requirements)
import Swarm.Language.Syntax
import Swarm.Language.Types
import Swarm.Language.Value as V
import Swarm.Util.JSON (optionsMinimize)

------------------------------------------------------------
-- Frames and continuations
------------------------------------------------------------

-- | A frame is a single component of a continuation stack, explaining
--   what to do next after we finish evaluating the currently focused
--   term.
data Frame
  = -- | We were evaluating the first component of a pair; next, we
    --   should evaluate the second component which was saved in this
    --   frame (and push a 'FFst' frame on the stack to save the first component).
    FSnd Term Env
  | -- | We were evaluating the second component of a pair; when done,
    --   we should combine it with the value of the first component saved
    --   in this frame to construct a fully evaluated pair.
    FFst Value
  | -- | @FArg t e@ says that we were evaluating the left-hand side of
    -- an application, so the next thing we should do is evaluate the
    -- term @t@ (the right-hand side, /i.e./ argument of the
    -- application) in environment @e@.  We will also push an 'FApp'
    -- frame on the stack.
    FArg Term Env
  | -- | @FApp v@ says that we were evaluating the right-hand side of
    -- an application; once we are done, we should pass the resulting
    -- value as an argument to @v@.
    FApp Value
  | -- | @FLet x ty t2 e@ says that we were evaluating a term @t1@ of
    -- type @ty@ in an expression of the form @let x = t1 in t2@, that
    -- is, we were evaluating the definition of @x@; the next thing we
    -- should do is evaluate @t2@ in the environment @e@ extended with
    -- a binding for @x@.
    FLet Var (Maybe (Polytype, Requirements)) Term Env
  | -- | We are executing inside a 'Try' block.  If an exception is
    --   raised, we will execute the stored term (the "catch" block).
    FTry Value
  | -- | An @FExec@ frame means the focused value is a command, which
    -- we should now execute.
    FExec
  | -- | We are in the process of executing the first component of a
    --   bind; once done, we should also execute the second component
    --   in the given environment (extended by binding the variable,
    --   if there is one, to the output of the first command).
    FBind (Maybe Var) (Maybe (Polytype, Requirements)) Term Env
  | -- | Apply specific updates to the world and current robot.
    --
    -- The 'Const' is used to track the original command for error messages.
    FImmediate Const [WorldUpdate Entity] [RobotUpdate]
  | -- | Update the memory cell at a certain location with the computed value.
    FUpdate Addr
  | -- | Signal that we are done with an atomic computation.
    FFinishAtomic
  | -- | We are in the middle of running a computation for all the
    --   nearby robots.  We have the function to run, and the list of
    --   robot IDs to run it on.
    FMeetAll Value [Int]
  | -- | We are in the middle of evaluating a record: some fields have
    --   already been evaluated; we are focusing on evaluating one
    --   field; and some fields have yet to be evaluated.
    FRcd Env [(Var, Value)] Var [(Var, Maybe Term)]
  | -- | We are in the middle of evaluating a record field projection.
    FProj Var
  | -- | We should suspend once we finish the current evaluation.
    FSuspend Env
  deriving (Eq, Show, Generic)

instance ToJSON Frame where
  toJSON = genericToJSON optionsMinimize

instance FromJSON Frame where
  parseJSON = genericParseJSON optionsMinimize

-- | A continuation is just a stack of frames.
type Cont = [Frame]

------------------------------------------------------------
-- Store
------------------------------------------------------------

type Addr = Int

-- | 'Store' represents a store, /i.e./ memory, indexing integer
--   locations to 'MemCell's.
data Store = Store {next :: Addr, mu :: IntMap MemCell}
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | A memory cell can be in one of three states.
data MemCell
  = -- | A cell starts out life as an unevaluated term together with
    --   its environment.
    E Term Env
  | -- | When the cell is 'Force'd, it is set to a 'Blackhole' while
    --   being evaluated.  If it is ever referenced again while still
    --   a 'Blackhole', that means it depends on itself in a way that
    --   would trigger an infinite loop, and we can signal an error.
    --   (Of course, we
    --   <http://www.lel.ed.ac.uk/~gpullum/loopsnoop.html cannot
    --   detect /all/ infinite loops this way>.)
    --
    --   A 'Blackhole' saves the original 'Term' and 'Env' that are
    --   being evaluated; if Ctrl-C is used to cancel a computation
    --   while we are in the middle of evaluating a cell, the
    --   'Blackhole' can be reset to 'E'.
    Blackhole Term Env
  | -- | Once evaluation is complete, we cache the final 'Value' in
    --   the 'MemCell', so that subsequent lookups can just use it
    --   without recomputing anything.
    V Value
  deriving (Show, Eq, Generic)

instance ToJSON MemCell where
  toJSON = genericToJSON optionsMinimize

instance FromJSON MemCell where
  parseJSON = genericParseJSON optionsMinimize

emptyStore :: Store
emptyStore = Store 0 IM.empty

-- | Allocate a new memory cell containing an unevaluated expression
--   with the current environment.  Return the index of the allocated
--   cell.
allocate :: Env -> Term -> Store -> (Addr, Store)
allocate e t (Store n m) = (n, Store (n + 1) (IM.insert n (E t e) m))

-- | Look up the cell at a given index.
lookupStore :: Addr -> Store -> Maybe MemCell
lookupStore n = IM.lookup n . mu

-- | Set the cell at a given index.
setStore :: Addr -> MemCell -> Store -> Store
setStore n c (Store nxt m) = Store nxt (IM.insert n c m)

------------------------------------------------------------
-- CESK machine
------------------------------------------------------------

-- | The overall state of a CESK machine, which can actually be one of
--   four kinds of states. The CESK machine is named after the first
--   kind of state, and it would probably be possible to inline a
--   bunch of things and get rid of the second state, but I find it
--   much more natural and elegant this way.  Most tutorial
--   presentations of CEK/CESK machines only have one kind of state, but
--   then again, most tutorial presentations only deal with the bare
--   lambda calculus, so one can tell whether a term is a value just
--   by seeing whether it is syntactically a lambda.  I learned this
--   approach from Harper's Practical Foundations of Programming
--   Languages.
data CESK
  = -- | When we are on our way "in/down" into a term, we have a
    --   currently focused term to evaluate in the environment, a store,
    --   and a continuation.  In this mode we generally pattern-match on the
    --   'Term' to decide what to do next.
    In Term Env Store Cont
  | -- | Once we finish evaluating a term, we end up with a 'Value'
    --   and we switch into "out" mode, bringing the value back up
    --   out of the depths to the context that was expecting it.  In
    --   this mode we generally pattern-match on the 'Cont' to decide
    --   what to do next.
    --
    --   Note that there is no 'Env', because we don't have anything
    --   with variables to evaluate at the moment, and we maintain the
    --   invariant that any unevaluated terms buried inside a 'Value'
    --   or 'Cont' must carry along their environment with them.
    Out Value Store Cont
  | -- | An exception has been raised.  Keep unwinding the
    --   continuation stack (until finding an enclosing 'Try' in the
    --   case of a command failure or a user-generated exception, or
    --   until the stack is empty in the case of a fatal exception).
    Up Exn Store Cont
  | -- | The machine is waiting for the game to reach a certain time
    --   to resume its execution.
    Waiting TickNumber CESK
  | -- | The machine is suspended, i.e. waiting for another term to
    --   evaluate.  This happens after we have evaluated whatever the
    --   user entered at the REPL and we are waiting for them to type
    --   something else.  Conceptually, this is like a combination of
    --   'Out' and 'In': we store a 'Value' that was just yielded by
    --   evaluation, and otherwise it is just like 'In' with a hole
    --   for the 'Term' we are going to evaluate.
    Suspended Value Env Store Cont
  deriving (Eq, Show, Generic)

instance ToJSON CESK where
  toJSON = genericToJSON optionsMinimize

instance FromJSON CESK where
  parseJSON = genericParseJSON optionsMinimize

-- | Is the CESK machine in a final (finished) state?  If so, extract
--   the final value and store.
finalValue :: CESK -> Maybe (Value, Store)
{-# INLINE finalValue #-}
finalValue (Out v s []) = Just (v, s)
finalValue (Suspended v _ s _) = Just (v, s) -- XXX is this correct?
finalValue _ = Nothing

-- XXX comment me
suspendedEnv :: Traversal' CESK Env
suspendedEnv = traversal go
 where
  go :: Applicative f => (Env -> f Env) -> CESK -> f CESK
  go f (Suspended v e s k) = Suspended v <$> f e <*> pure s <*> pure k
  go _ cesk = pure cesk

-- XXX comment me.  Better name?
initEmptyMachine :: TSyntax -> CESK
initEmptyMachine pt = initMachine pt mempty emptyStore

-- | Initialize a machine state with a starting term along with its
--   type; the term will be executed or just evaluated depending on
--   whether it has a command type or not.
initMachine :: TSyntax -> Env -> Store -> CESK
initMachine t e s = initMachine' t e s []

-- | Like 'initMachine', but also take an explicit starting continuation.
initMachine' :: TSyntax -> Env -> Store -> Cont -> CESK
initMachine' pt e s k =
  case pt ^. sType of
    -- XXX need to look through type synonyms here?
    -- If the starting term has a command type, create a machine that will
    -- evaluate and then execute it.
    Forall _ (TyCmd _) -> In t e s (FExec : k)
    -- Otherwise, for a term with a non-command type, just
    -- create a machine to evaluate it.
    _ -> In t e s k
 where
  -- Erase all type and SrcLoc annotations from the term before
  -- putting it in the machine state, since those are irrelevant at
  -- runtime.
  t = eraseS pt

-- | Load a program into an existing robot CESK machine: either continue from a
--   suspended state, or start from scratch with an empty environment.
continue :: TSyntax -> CESK -> CESK
continue t = \case
  Suspended _ e s k -> In (eraseS t) e s k -- XXX is this correct?  What about
  -- cmd vs non-cmd?  Do we need to
  -- add FExec frame?
  _ -> initMachine t mempty emptyStore

-- | Cancel the currently running computation.
cancel :: CESK -> CESK
cancel cesk = Out VUnit s' []
 where
  s' = resetBlackholes $ getStore cesk
  getStore (In _ _ s _) = s
  getStore (Out _ s _) = s
  getStore (Up _ s _) = s
  getStore (Waiting _ c) = getStore c
  getStore (Suspended _ _ s _) = s

-- | Reset any 'Blackhole's in the 'Store'.  We need to use this any
--   time a running computation is interrupted, either by an exception
--   or by a Ctrl+C.
resetBlackholes :: Store -> Store
resetBlackholes (Store n m) = Store n (IM.map resetBlackhole m)
 where
  resetBlackhole (Blackhole t e) = E t e
  resetBlackhole c = c

------------------------------------------------------------
-- Pretty printing CESK machine states
------------------------------------------------------------

instance PrettyPrec CESK where
  prettyPrec _ = \case
    In c _ _ k -> prettyCont k (11, "▶" <> ppr c <> "◀")
    Out v _ k -> prettyCont k (11, "◀" <> ppr (valueToTerm v) <> "▶")
    Up e _ k -> prettyCont k (11, "!" <> (pretty (formatExn mempty e) <> "!"))
    Waiting t cesk -> "🕑" <> pretty t <> "(" <> ppr cesk <> ")"
    Suspended v _ _ k -> prettyCont k (11, "◀" <> ppr (valueToTerm v) <> "...▶")

-- | Take a continuation, and the pretty-printed expression which is
--   the focus of the continuation (i.e. the expression whose value
--   will be given to the continuation) along with its top-level
--   precedence, and pretty-print the whole thing.
--
--   As much as possible, we try to print to look like an *expression*
--   with a currently focused part, that is, we print the continuation
--   from the inside out instead of as a list of frames.  This makes
--   it much more intuitive to read.
prettyCont :: Cont -> (Int, Doc ann) -> Doc ann
prettyCont [] (_, inner) = inner
prettyCont (f : k) inner = prettyCont k (prettyFrame f inner)

-- | Pretty-print a single continuation frame, given its already
--   pretty-printed focus.  In particular, given a frame and its
--   "inside" (i.e. the expression or other frames being focused on,
--   whose value will eventually be passed to this frame), with the
--   precedence of the inside's top-level construct, return a
--   pretty-printed version of the entire frame along with its
--   top-level precedence.
prettyFrame :: Frame -> (Int, Doc ann) -> (Int, Doc ann)
prettyFrame f (p, inner) = case f of
  FSnd t _ -> (11, "(" <> inner <> "," <+> ppr t <> ")")
  FFst v -> (11, "(" <> ppr (valueToTerm v) <> "," <+> inner <> ")")
  FArg t _ -> (10, pparens (p < 10) inner <+> prettyPrec 11 t)
  FApp v -> (10, prettyPrec 10 (valueToTerm v) <+> pparens (p < 11) inner)
  FLet x _ t _ -> (11, hsep ["let", pretty x, "=", inner, "in", ppr t])
  FTry v -> (10, "try" <+> pparens (p < 11) inner <+> prettyPrec 11 (valueToTerm v))
  FExec -> prettyPrefix "E·" (p, inner)
  FBind Nothing _ t _ -> (0, pparens (p < 1) inner <+> ";" <+> ppr t)
  FBind (Just x) _ t _ -> (0, hsep [pretty x, "<-", pparens (p < 1) inner, ";", ppr t])
  FImmediate c _worldUpds _robotUpds -> prettyPrefix ("I[" <> ppr c <> "]·") (p, inner)
  FUpdate addr -> prettyPrefix ("S@" <> pretty addr) (p, inner)
  FFinishAtomic -> prettyPrefix "A·" (p, inner)
  FMeetAll _ _ -> prettyPrefix "M·" (p, inner)
  FRcd _ done foc rest -> (11, encloseSep "[" "]" ", " (pDone ++ [pFoc] ++ pRest))
   where
    pDone = map (\(x, v) -> pretty x <+> "=" <+> ppr (valueToTerm v)) (reverse done)
    pFoc = pretty foc <+> "=" <+> inner
    pRest = map pprEq rest
    pprEq (x, Nothing) = pretty x
    pprEq (x, Just t) = pretty x <+> "=" <+> ppr t
  FProj x -> (11, pparens (p < 11) inner <> "." <> pretty x)
  FSuspend _ -> (10, "suspend" <+> pparens (p < 11) inner)

-- | Pretty-print a special "prefix application" frame, i.e. a frame
--   formatted like @X· inner@.  Unlike typical applications, these
--   associate to the *right*, so that we can print something like @X·
--   Y· Z· inner@ with no parens.
prettyPrefix :: Doc ann -> (Int, Doc ann) -> (Int, Doc ann)
prettyPrefix pre (p, inner) = (11, pre <+> pparens (p < 11) inner)

--------------------------------------------------------------
-- Runtime robot update
--------------------------------------------------------------

-- | Enumeration of robot updates.  This type is used for changes by
--   /e.g./ the @drill@ command which must be carried out at a later
--   tick. Using a first-order representation (as opposed to /e.g./
--   just a @Robot -> Robot@ function) allows us to serialize and
--   inspect the updates.
--
--   Note that this can not be in 'Swarm.Game.Robot' as it would create
--   a cyclic dependency.
data RobotUpdate
  = -- | Add copies of an entity to the robot's inventory.
    AddEntity Count Entity
  | -- | Make the robot learn about an entity.
    LearnEntity Entity
  deriving (Eq, Ord, Show, Generic)

instance ToJSON RobotUpdate where
  toJSON = genericToJSON optionsMinimize

instance FromJSON RobotUpdate where
  parseJSON = genericParseJSON optionsMinimize
