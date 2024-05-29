{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types and utilities for working with "universal locations";
-- locations that encompass different 2-D subworlds.
module Swarm.Game.Universe where

import Control.Lens (makeLenses, view)
import Data.Function (on)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Yaml (FromJSON, ToJSON, Value (Object), parseJSON, withText, (.:))
import GHC.Generics (Generic)
import Linear (V2 (..))
import Swarm.Game.Location
import Swarm.Util (quote)

-- * Referring to subworlds

data SubworldName = DefaultRootSubworld | SubworldName Text
  deriving (Show, Eq, Ord, Generic, ToJSON)

instance FromJSON SubworldName where
  parseJSON = withText "subworld name" $ return . SubworldName

renderWorldName :: SubworldName -> Text
renderWorldName = \case
  SubworldName s -> s
  DefaultRootSubworld -> "<default>"

renderQuotedWorldName :: SubworldName -> Text
renderQuotedWorldName = \case
  SubworldName s -> quote s
  DefaultRootSubworld -> "<default>"

-- * Universal location

-- | The swarm universe consists of locations
-- indexed by subworld.
-- Not only is this parameterized datatype useful for planar (2D)
-- coordinates, but is also used for named waypoints.
data Cosmic a = Cosmic
  { _subworld :: SubworldName
  , _planar :: a
  }
  deriving (Show, Eq, Ord, Functor, Generic, ToJSON)

makeLenses ''Cosmic

instance (FromJSON a) => FromJSON (Cosmic a) where
  parseJSON x = case x of
    Object v -> objParse v
    _ -> Cosmic DefaultRootSubworld <$> parseJSON x
   where
    objParse v =
      Cosmic
        <$> v .: "subworld"
        <*> v .: "loc"

-- * Measurement

data DistanceMeasure b = Measurable b | InfinitelyFar
  deriving (Eq, Ord)

getFiniteDistance :: DistanceMeasure b -> Maybe b
getFiniteDistance = \case
  Measurable x -> Just x
  InfinitelyFar -> Nothing

-- | Returns 'InfinitelyFar' if not within the same subworld.
cosmoMeasure :: (a -> a -> b) -> Cosmic a -> Cosmic a -> DistanceMeasure b
cosmoMeasure f a b
  | ((/=) `on` view subworld) a b = InfinitelyFar
  | otherwise = Measurable $ (f `on` view planar) a b

-- * Utilities

defaultCosmicLocation :: Cosmic Location
defaultCosmicLocation = Cosmic DefaultRootSubworld origin

offsetBy :: Cosmic Location -> V2 Int32 -> Cosmic Location
offsetBy loc v = fmap (.+^ v) loc
