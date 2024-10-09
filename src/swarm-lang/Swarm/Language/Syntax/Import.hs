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
  PathStatus (..),
  ImportDir,
  mkImportDir,
  withImportDir,

  -- ** Pre-defined dirs
  homeDir,
  currentDir,

  -- * ImportLoc
  ImportLoc (..),
  importAnchor,

  -- * Canonicalization
  forgetCanonical,
  canonicalizeImportDir,
  canonicalizeImportLoc,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
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

-- XXX get rid of phantom type parameter --- just don't export
-- constructor and export only a smart constructor that does the
-- canonicalization

-- | 'PathStatus' values are intended to be used as a phantom type
--   parameter to track whether an import location has just been
--   parsed or has been canonicalized, so we can't accidentally forget
--   to canonicalize a path.
data PathStatus = Parsed | Canonical
  deriving (Eq)

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
--   deliberate choice to enforce safety: it is only possible to
--   create an @'ImportDir' 'Parsed'@ with 'mkImportDir', and then one
--   can use 'canonicalizeImportDir' (or 'canonicalizeImportLoc') to
--   turn it into an @'ImportDir' 'Canonical'@.  To pattern-match on
--   an 'ImportDir', use 'withImportDir'.
data ImportDir (c :: PathStatus) = ImportDir Anchor [Text]
  deriving (Eq, Ord, Show, Data, Generic, FromJSON, ToJSON)

-- | Convenient shortcut for the 'ImportDir' representing the user's
--   home directory.
homeDir :: ImportDir Canonical
homeDir = ImportDir Home []

-- | Convenient shortcut for the 'ImportDir' representing the current
--   working directory.
currentDir :: ImportDir Canonical
currentDir = ImportDir (Local 0) []

-- | Constructor for 'ImportDir Parsed'.  We do not export the actual
--   constructor to preserve the invariant on the type index: the only
--   way to get an 'ImportDir Canonical' is via 'canonicalizeImportDir'.
mkImportDir :: Anchor -> [Text] -> ImportDir Parsed
mkImportDir = ImportDir

-- | Destructor/eliminator for 'ImportDir'.  Since the constructor for
--   'ImportDir' is not exported, this is the primary way to access
--   the contents.
withImportDir :: (Anchor -> [Text] -> r) -> ImportDir c -> r
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
instance Semigroup (ImportDir Canonical) where
  _ <> d@(ImportDir (Web {}) _) = d
  _ <> d@(ImportDir Home _) = d
  _ <> d@(ImportDir Absolute _) = d
  ImportDir a p1 <> ImportDir (Local n) p2 = canonicalizeImportDir (ImportDir a (p1 ++ replicate n ".." ++ p2))

-- | The identity 'ImportDir' is one with a local anchor and empty
--   path component list.
instance Monoid (ImportDir Canonical) where
  mempty = ImportDir (Local 0) []

------------------------------------------------------------
-- ImportLoc

-- | A location from which to import a file containing Swarm code,
--   consisting of a directory paired with a filename.
data ImportLoc c = ImportLoc {importDir :: ImportDir c, importFile :: Text}
  deriving (Eq, Ord, Show, Data, Generic, FromJSON, ToJSON)

-- | Get the 'Anchor' for an 'ImportLoc'.
importAnchor :: ImportLoc Canonical -> Anchor
importAnchor = withImportDir const . importDir

------------------------------------------------------------
-- Canonicalization

-- | Forget the 'Canonical' index on an 'ImportDir' value, turning it
--   back into @'ImportDir' 'Parsed'@.
forgetCanonical :: ImportDir Canonical -> ImportDir Parsed
forgetCanonical = coerce

-- | Canonicalize an import directory.  A canonical import directory
--   never contains ".." (except at the beginning in the form of a
--   'Local' 'Anchor') or ".".
--
-- >>> canonicalizeImportDir (ImportDir Home ["."])
-- ImportDir {importAnchor = Home, importPath = []}
-- >>> canonicalizeImportDir (ImportDir Home ["a","b"])
-- ImportDir {importAnchor = Home, importPath = ["a","b"]}
-- >>> canonicalizeImportDir (ImportDir Home ["a","..","b"])
-- ImportDir {importAnchor = Home, importPath = ["b"]}
-- >>> canonicalizeImportDir (ImportDir Home ["a","..","..","b"])
-- ImportDir {importAnchor = Home, importPath = ["b"]}
-- >>> canonicalizeImportDir (ImportDir (Local 0) ["a", "..", "b"])
-- ImportDir {importAnchor = Local 0, importPath = ["b"]}
-- >>> canonicalizeImportDir (ImportDir (Local 0) ["a", "..", "..", "b"])
-- ImportDir {importAnchor = Local 1, importPath = ["b"]}
-- >>> canonicalizeImportDir (ImportDir (Local 0) ["a", "..", "..", "b", ".", "c"])
-- ImportDir {importAnchor = Local 1, importPath = ["b","c"]}
-- >>> canonicalizeImportDir (ImportDir (Local 1) ["a", "..", "..", "b", "c", "..", "d", "..", "..", ".."])
-- ImportDir {importAnchor = Local 3, importPath = []}
canonicalizeImportDir :: ImportDir Parsed -> ImportDir Canonical
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

-- | Canonicalize an import location, by canonicalizing the import
--   directory it contains (if any).
canonicalizeImportLoc :: ImportLoc Parsed -> ImportLoc Canonical
canonicalizeImportLoc (ImportLoc d f) = ImportLoc (canonicalizeImportDir d) f
