{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- XXX
module Swarm.Language.Syntax.Import (
  PathStatus (Parsed),
  ImportDir (..),
  ImportLoc (..),
  canonicalizeImportDir,
  canonicalizeImportLoc,
) where

import Data.Data
import Data.Text (Text)
import GHC.Generics (Generic)

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
  deriving (Eq, Show, Data, Generic)

-- XXX facility for creating import dir, make sure it is canonicalized
-- e.g. what if .. or . shows up in the middle

-- XXX
data PathStatus = Parsed | Canonical
  deriving (Eq)

-- | An import directory consists of an 'Anchor' together with a path.
--
--   Note that the path is stored in /reverse/ order, with the last
--   path component (the one closest to the file) stored first. This
--   makes it easier to process things like ".." parent components.
--
--   The decision to represent directories as a list of
--   components, rather than using e.g. @FilePath@ or @OsPath@, is
--   deliberate: it allows us to decouple the syntax of import
--   specifications in Swarm-lang from the actual OS-specific path
--   syntax.  For example, we can allow using either front- or
--   backslashes as path separators in import expressions regardless
--   of the OS, then turn it into an actual OS path in an appropriate
--   way.
data ImportDir (c :: PathStatus) = ImportDir {importAnchor :: Anchor, importPath :: [Text]}
  deriving (Eq, Show, Data, Generic)

-- The Semigroup instance for 'ImportDir' interprets the second in the
-- context of the first.  If the second 'ImportDir' is not local
-- (/i.e./ if its anchor is 'Web', 'Home', or 'Absolute'), the first
-- is simply ignored and the second is returned unchanged.  If the
-- second 'ImportDir' is 'Local', a location is returned which uses
-- the first location as an initial anchor and then interprets the
-- second relative to that.
--
-- See https://github.com/dhall-lang/dhall-lang/blob/master/standard/imports.md#chaining-directories
-- for inspiration.
instance Semigroup (ImportDir Canonical) where
  _ <> d@(ImportDir (Web {}) _) = d
  _ <> d@(ImportDir Home _) = d
  _ <> d@(ImportDir Absolute _) = d
  ImportDir a p1 <> ImportDir (Local 0) p2 = ImportDir a (p2 <> p1)
  ImportDir a (_ : ps1) <> ImportDir (Local n) p2 = ImportDir a ps1 <> ImportDir (Local (n - 1)) p2
  ImportDir (Local m) [] <> ImportDir (Local n) p2 = ImportDir (Local (m + n)) p2
  ImportDir a [] <> ImportDir (Local _) p2 = ImportDir a p2

instance Monoid (ImportDir Canonical) where
  mempty = ImportDir (Local 0) []

-- | A location from which to import a file containing Swarm code,
--   consisting of a directory paired with a filename.
data ImportLoc c = ImportLoc {importDir :: ImportDir c, importFile :: Text}
  deriving (Eq, Show, Data, Generic)

canonicalizeImportDir :: ImportDir Parsed -> ImportDir Canonical
canonicalizeImportDir = undefined

canonicalizeImportLoc :: ImportLoc Parsed -> ImportLoc Canonical
canonicalizeImportLoc = undefined
