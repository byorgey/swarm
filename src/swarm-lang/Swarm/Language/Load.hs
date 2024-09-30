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
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.State (State, get, modify)
import Control.Effect.Throw (Throw, throwError)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Swarm.Failure (Asset (..), AssetData (..), Entry (..), LoadingFailure (..), SystemFailure (AssetNotLoaded))
import Swarm.Language.Parser (readTerm')
import Swarm.Language.Parser.Core (defaultParserConfig)
import Swarm.Language.Syntax (ImportDir, ImportLocation (..), Syntax)
import Swarm.Util (readFileMay)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Witch (into)

-- | A SourceMap associates canonical 'ImportLocation's to parsed
--   ASTs.  There's no particular reason to require an imported module
--   to be nonempty, so we allow it.
type SourceMap = Map ImportLocation (Maybe Syntax)

doesLocationExist :: (Has (Lift IO) sig m) => ImportLocation -> m Bool
doesLocationExist (ImportLocation dir f) = sendIO $ doesFileExist (dir </> f)

-- | XXX edit this
--   Fully resolve/canonicalize implicitly specified import locations,
--   relative to a given base import location.
--
--   For example, when importing it is
--   allowed to omit a trailing @.sw@ extension; resolving will add
--   the extension.
resolveImportLocation ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  ImportDir ->
  ImportLocation ->
  m ImportLocation
resolveImportLocation parent loc = undefined

-- XXX copied this code from the code for executing Run.
-- Need to first move Swarm.Game.ResourceLoading to Swarm.ResourceLoading in swarm-util,
-- so it will be accessible here.

-- sData <- throwToMaybe @SystemFailure $ getDataFileNameSafe Script filePath
-- sDataSW <- throwToMaybe @SystemFailure $ getDataFileNameSafe Script (filePath <> ".sw")
-- mf <- sendIO $ mapM readFileMay $ [filePath, filePath <> ".sw"] <> catMaybes [sData, sDataSW]

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
  canonicalLoc <- resolveImportLocation loc
  srcMap <- get @SourceMap
  case M.lookup canonicalLoc srcMap of
    Just _ -> pure () -- already loaded - do nothing
    Nothing -> do
      msrc <- sendIO $ readFileMay canonicalLoc
      case msrc of
        Nothing -> throwError $ AssetNotLoaded (Data Script) canonicalLoc (DoesNotExist File)
        Just src -> case readTerm' defaultParserConfig (into @Text src) of
          Left err -> throwError $ AssetNotLoaded (Data Script) canonicalLoc (CanNotParseMegaparsec err)
          Right t -> do
            modify @SourceMap (M.insert canonicalLoc t)

-- XXX enumerate imports and recursively load them.
-- XXX need to resolve imports relative to location of the file that imported them
-- XXX recursively load imports
