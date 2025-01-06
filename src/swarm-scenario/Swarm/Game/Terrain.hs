{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Terrain types and properties.
module Swarm.Game.Terrain (
  TerrainType (..),
  TerrainObj (..),
  TerrainMap (..),
  blankTerrainIndex,
  getTerrainDefaultPaletteChar,
  getTerrainWord,
  terrainFromText,
  loadTerrain,
  mkTerrainMap,
  validateTerrainAttrRefs,
) where

import Control.Algebra (Has)
import Control.Arrow (first, (&&&))
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Throw (Throw, liftEither, throwError)
import Control.Monad (forM, unless, (<=<))
import Data.Char (toUpper)
import Data.Hashable (Hashable)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple (swap)
import Data.Yaml
import GHC.Generics (Generic)
import Swarm.Failure
import Swarm.Game.Display
import Swarm.Game.Entity.Cosmetic
import Swarm.ResourceLoading (getDataFileNameSafe)
import Swarm.Util (enumeratedMap, quote)
import Swarm.Util.Effect (withThrow)

data TerrainType = BlankT | TerrainType Text
  deriving (Eq, Ord, Show, Generic, ToJSON, Hashable)

blankTerrainIndex :: Int
blankTerrainIndex = 0

terrainFromText :: Text -> TerrainType
terrainFromText "blank" = BlankT
terrainFromText x = TerrainType x

getTerrainWord :: TerrainType -> Text
getTerrainWord BlankT = "blank"
getTerrainWord (TerrainType x) = x

instance FromJSON TerrainType where
  parseJSON =
    withText "TerrainType" $
      return . terrainFromText

instance Semigroup TerrainType where
  t <> BlankT = t
  _ <> t = t

instance Monoid TerrainType where
  mempty = BlankT

getTerrainDefaultPaletteChar :: TerrainType -> Char
getTerrainDefaultPaletteChar = toUpper . T.head . getTerrainWord

-- | Representation for parsing only. Not exported.
data TerrainItem = TerrainItem
  { name :: TerrainType
  , attr :: Text
  , description :: Text
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data TerrainObj = TerrainObj
  { terrainName :: TerrainType
  , terrainDesc :: Text
  , terrainDisplay :: Display
  }
  deriving (Show)

promoteTerrainObjects :: [TerrainItem] -> [TerrainObj]
promoteTerrainObjects =
  map f
 where
  f (TerrainItem n a d) =
    TerrainObj n d $
      (defaultTerrainDisplay (AWorld a))
        { _backgroundSources =
            BackgroundSource
              { terrainBgAttr = Just . WorldAttr . T.unpack $ getTerrainWord n
              , entityBgAttr = Nothing
              }
        }

invertedIndexMap :: IntMap TerrainObj -> Map TerrainType Int
invertedIndexMap = M.fromList . map (first terrainName . swap) . IM.toList

-- | Each terrain type shall have a unique
-- integral index. The indices should
-- be consecutive by parse order.
data TerrainMap = TerrainMap
  { terrainByName :: Map TerrainType TerrainObj
  , terrainByIndex :: IntMap TerrainObj
  , terrainIndexByName :: Map TerrainType Int
  -- ^ basically the inverse of 'terrainByIndex'.
  -- This needs to be (is) recomputed upon every update to
  -- the other fields in 'TerrainMap'.
  }
  deriving (Show)

instance Semigroup TerrainMap where
  TerrainMap oldByName oldByIndex _ <> TerrainMap newByName newByIndex _ =
    TerrainMap
      (oldByName <> newByName)
      combinedTerrainByIndex
      (invertedIndexMap combinedTerrainByIndex)
   where
    combinedTerrainByIndex = oldByIndex <> enumeratedMap (IM.size oldByIndex) (IM.elems newByIndex)

instance Monoid TerrainMap where
  mempty = TerrainMap mempty mempty mempty

mkTerrainMap :: [TerrainObj] -> TerrainMap
mkTerrainMap items =
  TerrainMap
    { terrainByName = M.fromList $ map (terrainName &&& id) items
    , terrainByIndex = byIndex
    , terrainIndexByName = invertedIndexMap byIndex
    }
 where
  byIndex = enumeratedMap blankTerrainIndex items

-- | Validates references to 'Display' attributes
validateTerrainAttrRefs :: Has (Throw LoadingFailure) sig m => Set WorldAttr -> [TerrainItem] -> m [TerrainObj]
validateTerrainAttrRefs validAttrs rawTerrains =
  forM rawTerrains $ \(TerrainItem n a d) -> do
    unless (Set.member (WorldAttr $ T.unpack a) validAttrs)
      . throwError
      . CustomMessage
      $ T.unwords
        [ "Nonexistent attribute"
        , quote a
        , "referenced by terrain"
        , quote $ getTerrainWord n
        ]

    return $ TerrainObj n d $ defaultTerrainDisplay (AWorld a)

-- | Load terrain from a data file called @terrains.yaml@, producing
--   either an 'TerrainMap' or a parse error.
loadTerrain ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  m TerrainMap
loadTerrain = do
  fileName <- getDataFileNameSafe Terrain terrainFile
  decoded <-
    withThrow (terrainFailure . CanNotParseYaml) . (liftEither <=< sendIO) $
      decodeFileEither fileName

  let terrainObjs = promoteTerrainObjects decoded
  -- Ensures that the blank terrain gets index 0
  return $ mkTerrainMap $ blankTerrainObj : terrainObjs
 where
  terrainFile = "terrains.yaml"
  terrainFailure = AssetNotLoaded (Data Terrain) terrainFile

  blankTerrainObj = TerrainObj BlankT "Blank terrain" $ defaultTerrainDisplay ADefault
