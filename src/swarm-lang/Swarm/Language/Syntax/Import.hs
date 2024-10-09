{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Data types to represent Swarm-lang import locations.
module Swarm.Language.Syntax.Import (
  -- * Anchors
  Anchor (..),

  -- * ImportDir
  ImportDir,
  mkImportDir,
  withImportDir,

  -- ** Pre-defined dirs
  homeDir,
  currentDir,

  -- * ImportLoc
  ImportLoc (..),
  importAnchor,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Text (Text)
import GHC.Generics (Generic)

------------------------------------------------------------
-- Anchor

-- | An "anchor" from which to interpret a path.
data Anchor where
  -- | Text represents scheme + authority, /e.g./ "https://github.com"
  Web :: Text -> Anchor
  -- | Relative to local dir.  The @Int@ is the
  --   number of levels to go up, *e.g.* 2 means
  --   "../..".  0 means the current dir, /i.e./ "."
  Local :: Int -> Anchor
  -- | Relative to the home directory, /i.e./ "~"
  Home :: Anchor
  -- | Absolute, /i.e./ relative to the filesystem root.
  Absolute :: Anchor
  deriving (Eq, Ord, Show, Data, Generic, FromJSON, ToJSON)

------------------------------------------------------------
-- ImportDir

-- | An import directory consists of an 'Anchor' together with a path.
--
--   The decision to represent directories as a list of
--   components, rather than using e.g. @FilePath@ or @OsPath@, is
--   deliberate: it allows us to decouple the syntax of import
--   specifications in Swarm-lang from the actual OS-specific path
--   syntax.  For example, we can allow using either front- or
--   backslashes as path separators in import expressions regardless
--   of the OS, then turn it into an actual OS path in an appropriate
--   way.
--
--   The constructor of 'ImportDir' is not exported.  This is a
--   deliberate choice to enforce safety: we enforce as an invariant
--   that 'ImportDir' values are always normalized.  To create one,
--   use the 'mkImportDir' smart constructor. To extract information,
--   use 'withImportDir'.
data ImportDir = ImportDir Anchor [Text]
  deriving (Eq, Ord, Show, Data, Generic, FromJSON, ToJSON)

-- | Convenient shortcut for the 'ImportDir' representing the user's
--   home directory.
homeDir :: ImportDir
homeDir = ImportDir Home []

-- | Convenient shortcut for the 'ImportDir' representing the current
--   working directory.
currentDir :: ImportDir
currentDir = ImportDir (Local 0) []

-- | Smart constructor for 'ImportDir' which ensures that it is
-- normalized.
mkImportDir :: Anchor -> [Text] -> ImportDir
mkImportDir a p = canonicalizeImportDir $ ImportDir a p

-- | Destructor/eliminator for 'ImportDir'.  Since the constructor for
--   'ImportDir' is not exported, this is the primary way to access
--   the contents.
withImportDir :: (Anchor -> [Text] -> r) -> ImportDir -> r
withImportDir f (ImportDir a p) = f a p

-- | The Semigroup instance for 'ImportDir' interprets the second in
--   the context of the first.  If the second 'ImportDir' is not local
--   (/i.e./ if its anchor is 'Web', 'Home', or 'Absolute'), the first
--   is simply ignored and the second is returned unchanged.  If the
--   second 'ImportDir' is 'Local', computes a location which uses the
--   first location as an initial anchor and then interprets the
--   second relative to that.
--
--   See
--   https://github.com/dhall-lang/dhall-lang/blob/master/standard/imports.md#chaining-directories
--   for inspiration.
instance Semigroup ImportDir where
  _ <> d@(ImportDir (Web {}) _) = d
  _ <> d@(ImportDir Home _) = d
  _ <> d@(ImportDir Absolute _) = d
  ImportDir a p1 <> ImportDir (Local n) p2 = mkImportDir a (p1 ++ replicate n ".." ++ p2)

-- | The identity 'ImportDir' is one with a local anchor and empty
--   path component list.
instance Monoid ImportDir where
  mempty = ImportDir (Local 0) []

------------------------------------------------------------
-- ImportLoc

-- | A location from which to import a file containing Swarm code,
--   consisting of a directory paired with a filename.
data ImportLoc = ImportLoc {importDir :: ImportDir, importFile :: Text}
  deriving (Eq, Ord, Show, Data, Generic, FromJSON, ToJSON)

-- | Get the 'Anchor' for an 'ImportLoc'.
importAnchor :: ImportLoc -> Anchor
importAnchor = withImportDir const . importDir

------------------------------------------------------------
-- Canonicalization

-- | Canonicalize an import directory.  A canonical import directory
--    never contains ".." (except at the beginning in the form of a
--    'Local' 'Anchor') or ".".
--
--   This is not meant to be exported, but is used as part of the
--   smart constructor 'mkImportDir'.
--
--  >>> canonicalizeImportDir (ImportDir Home ["."])
--  ImportDir {importAnchor = Home, importPath = []}
--  >>> canonicalizeImportDir (ImportDir Home ["a","b"])
--  ImportDir {importAnchor = Home, importPath = ["a","b"]}
--  >>> canonicalizeImportDir (ImportDir Home ["a","..","b"])
--  ImportDir {importAnchor = Home, importPath = ["b"]}
--  >>> canonicalizeImportDir (ImportDir Home ["a","..","..","b"])
--  ImportDir {importAnchor = Home, importPath = ["b"]}
--  >>> canonicalizeImportDir (ImportDir (Local 0) ["a", "..", "b"])
--  ImportDir {importAnchor = Local 0, importPath = ["b"]}
--  >>> canonicalizeImportDir (ImportDir (Local 0) ["a", "..", "..", "b"])
--  ImportDir {importAnchor = Local 1, importPath = ["b"]}
--  >>> canonicalizeImportDir (ImportDir (Local 0) ["a", "..", "..", "b", ".", "c"])
--  ImportDir {importAnchor = Local 1, importPath = ["b","c"]}
--  >>> canonicalizeImportDir (ImportDir (Local 1) ["a", "..", "..", "b", "c", "..", "d", "..", "..", ".."])
--  ImportDir {importAnchor = Local 3, importPath = []}
canonicalizeImportDir :: ImportDir -> ImportDir
canonicalizeImportDir (ImportDir a p) = case (a, canonicalizeDir (0, [], p)) of
  (Local n, (m, p')) -> ImportDir (Local (n + m)) p'
  (_, (_, p')) -> ImportDir a p'
 where
  canonicalizeDir :: (Int, [Text], [Text]) -> (Int, [Text])
  canonicalizeDir = \case
    (n, pre, "." : ps) -> canonicalizeDir (n, pre, ps)
    (n, [], ".." : ps) -> canonicalizeDir (n + 1, [], ps)
    (n, _ : xs, ".." : ps) -> canonicalizeDir (n, xs, ps)
    (n, pre, d : ps) -> canonicalizeDir (n, d : pre, ps)
    (n, pre, []) -> (n, reverse pre)
