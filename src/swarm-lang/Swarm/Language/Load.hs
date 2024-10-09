{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Loading Swarm modules from disk or network, recursively loading
-- any imports.
module Swarm.Language.Load (
  dirToFilePath,
  locToFilePath,
  resolveImportLoc,
  load,
  loadWith,
) where

import Control.Algebra (Has)
import Control.Carrier.State.Strict (execState)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.State (State, get, modify)
import Control.Effect.Throw (Throw, throwError)
import Control.Lens (universe, view)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Failure (Asset (..), AssetData (..), Entry (..), LoadingFailure (..), SystemFailure (AssetNotLoaded))
import Swarm.Language.Parser (readTerm')
import Swarm.Language.Parser.Core (defaultParserConfig)
import Swarm.Language.Syntax.Import (Anchor (..), ImportDir, ImportLoc (..), PathStatus (..), currentDir, importAnchor, withImportDir)
import Swarm.Language.Syntax.Pattern (Syntax, TImportIn, Term, sTerm)
import Swarm.Util (readFileMay, readFileMayT)
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.FilePath (joinPath, splitPath, (</>))
import Witch (into)

------------------------------------------------------------
-- Import location utilities

-- | Turn an 'Anchor' into a concrete 'FilePath' (or URL).
anchorToFilePath :: (Has (Lift IO) sig m) => Anchor -> m FilePath
anchorToFilePath = \case
  Web w -> pure $ into @FilePath w
  Local n -> local n <$> sendIO getCurrentDirectory
  Home -> sendIO getHomeDirectory
  Absolute -> pure "/"
 where
  local :: Int -> FilePath -> FilePath
  local n = ("/" </>) . joinPath . reverse . drop n . reverse . splitPath

-- | Turn an 'ImportDir' into a concrete 'FilePath' (or URL).
dirToFilePath :: (Has (Lift IO) sig m) => ImportDir Canonical -> m FilePath
dirToFilePath = withImportDir $ \a p -> do
  af <- anchorToFilePath a
  pure $ af </> joinPath (map (into @FilePath) p)

-- | Turn an 'ImportLoc' into a concrete 'FilePath' (or URL).
locToFilePath :: (Has (Lift IO) sig m) => ImportLoc Canonical -> m FilePath
locToFilePath (ImportLoc d f) = do
  df <- dirToFilePath d
  pure $ df </> into @FilePath f

-- XXX simply assume web resources exist without checking?  + require them to be fully named...?

-- | Check whether a given 'ImportLoc' in fact exists.  Note that, for
--   the sake of efficiency, this simply assumes that any 'Web'
--   resource exists without checking; all other locations will
--   actually be checked.
doesLocationExist :: (Has (Lift IO) sig m) => ImportLoc Canonical -> m Bool
doesLocationExist loc = do
  fp <- locToFilePath loc
  case importAnchor (importDir loc) of
    Web {} -> pure True
    _ -> sendIO $ doesFileExist fp

-- XXX need to be able to resolve "local" to something in a standard Swarm data location??

-- | Fully resolve an implicitly specified import location, relative
--   to a given base directory, possibly appending @.sw@.
--
--   Note that URLs will /not/ have @.sw@ appended automatically.
resolveImportLoc ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  ImportDir Canonical ->
  ImportLoc Canonical ->
  m (ImportLoc Canonical)
resolveImportLoc parent (ImportLoc d f) = do
  e1 <- doesLocationExist loc'
  e2 <- doesLocationExist loc'sw
  case (e1, e2) of
    -- Only automatically add .sw extension if the original location
    -- does not exist, but the location with .sw appended does
    (False, True) -> pure loc'sw
    _ -> pure loc'
 where
  d' = parent <> d
  loc' = ImportLoc d' f
  loc'sw = ImportLoc d' (f <> ".sw")

-- | A SourceMap associates canonical 'ImportLocation's to parsed
--   ASTs.  There's no particular reason to require an imported module
--   to be nonempty, so we allow it.
type SourceMap = Map (ImportLoc Canonical) (Maybe Syntax)

-- XXX copied this code from the code for executing Run. Do we need to
-- deal with loading things from standard swarm script dirs, for
-- scenario code?  Or maybe we just make that a new type of anchor,
-- with new syntax?

-- sData <- throwToMaybe @SystemFailure $ getDataFileNameSafe Script filePath
-- sDataSW <- throwToMaybe @SystemFailure $ getDataFileNameSafe Script (filePath <> ".sw")
-- mf <- sendIO $ mapM readFileMay $ [filePath, filePath <> ".sw"] <> catMaybes [sData, sDataSW]

-- | Load and parse Swarm source code from a given location,
--   recursively loading and parsing any imports, ultimately returning
--   a 'SourceMap' from locations to parsed ASTs.
load ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  ImportLoc Canonical ->
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
  ImportLoc Canonical ->
  m SourceMap
loadWith srcMap = execState srcMap . loadRec currentDir

-- | XXX comment me
loadRec ::
  (Has (Throw SystemFailure) sig m, Has (State SourceMap) sig m, Has (Lift IO) sig m) =>
  ImportDir Canonical ->
  ImportLoc Canonical ->
  m ()
loadRec parent loc = do
  canonicalLoc <- resolveImportLoc parent loc
  srcMap <- get @SourceMap
  case M.lookup canonicalLoc srcMap of
    Just _ -> pure () -- already loaded - do nothing
    Nothing -> do
      src <- readLoc canonicalLoc
      path <- locToFilePath canonicalLoc
      case readTerm' defaultParserConfig src of
        Left err -> throwError $ AssetNotLoaded (Data Script) path (CanNotParseMegaparsec err)
        Right mt -> do
          modify @SourceMap (M.insert canonicalLoc mt)
          case mt of
            Nothing -> pure ()
            Just t -> do
              let recImports = enumerateImports t
              mapM_ (loadRec (importDir canonicalLoc)) recImports

enumerateImports :: Syntax -> [ImportLoc Canonical]
enumerateImports = mapMaybe getImportLoc . universe . view sTerm
 where
  getImportLoc :: Term -> Maybe (ImportLoc Canonical)
  getImportLoc (TImportIn loc _) = Just (canonicalizeImportLoc loc)
  getImportLoc _ = Nothing

readLoc ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  ImportLoc Canonical ->
  m Text
readLoc loc = do
  path <- locToFilePath loc
  case importAnchor loc of
    Web {} -> undefined -- XXX load URL with some kind of HTTP library
    _ -> sendIO (readFileMayT path) >>= maybe (throwError (notFound path)) pure
 where
  notFound path = AssetNotLoaded (Data Script) path (DoesNotExist File)
