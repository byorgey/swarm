-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Loading Swarm modules from disk or network, recursively loading
-- any imports.
module Swarm.Language.Load (
  load,
  loadWith,
) where

import Control.Algebra (Has)
import Control.Carrier.State.Strict (execState)
import Control.Effect.Lift (Lift)
import Control.Effect.State (State)
import Control.Effect.Throw (Throw)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Swarm.Failure (SystemFailure)
import Swarm.Language.Parser (readTerm')
import Swarm.Language.Parser.Core (defaultParserConfig)
import Swarm.Language.Syntax

-- | A SourceMap associates canonical 'ImportLocation's to parsed
--   ASTs.  There's no particular reason to require an imported module
--   to be nonempty, so we allow it.
type SourceMap = Map ImportLocation (Maybe Syntax)

-- | Fully resolve/canonicalize implicitly specified import locations,
--   while loading their content.  For example, when importing it is
--   allowed to omit a trailing @.sw@ extension; resolving will add
--   the extension.
resolveImportLocation ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  ImportLocation ->
  m (ImportLocation, Text)
resolveImportLocation = undefined

-- | Load and parse Swarm source code from a given location,
--   recursively loading and parsing any imports, ultimately returning
--   a 'SourceMap' from locations to parsed ASTs.
load ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  ImportLocation ->
  m SourceMap
load = loadWith M.empty

-- | Like 'load', but use an existing 'SourceMap' as a starting point.
--   Returns an updated 'SourceMap' which extends the existing one,
--   and is guaranteed to include the specified import as well as
--   anything it imports, recursively.
--
--   Any import locations which are already present in the 'SourceMap'
--   will /not/ be reloaded from the disk/network; only newly
--   encountered import locations will be loaded.  If you wish to
--   reload things from disk/network in case they have changed, use
--   'load' instead.
loadWith ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  SourceMap ->
  ImportLocation ->
  m SourceMap
loadWith srcMap = execState srcMap . loadRec

loadRec ::
  (Has (Throw SystemFailure) sig m, Has (State SourceMap) sig m, Has (Lift IO) sig m) =>
  ImportLocation ->
  m ()
loadRec loc = do
  (loc', src) <- resolveImportLocation loc
  case readTerm' defaultParserConfig src of
    Left err -> undefined
    Right s -> undefined
