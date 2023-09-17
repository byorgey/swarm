-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types for styling custom entity attributes
module Swarm.Game.Scenario.Style where

import Data.Aeson
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)

data StyleFlag
  = Standout
  | Italic
  | Strikethrough
  | Underline
  | ReverseVideo
  | Blink
  | Dim
  | Bold
  deriving (Eq, Ord, Show, Generic)

styleFlagJsonOptions :: Options
styleFlagJsonOptions =
  defaultOptions
    { sumEncoding = UntaggedValue
    }

instance FromJSON StyleFlag where
  parseJSON = genericParseJSON styleFlagJsonOptions

instance ToJSON StyleFlag where
  toJSON = genericToJSON styleFlagJsonOptions

-- | Hexadecimal color notation.
-- May include a leading hash symbol (see 'Data.Colour.SRGB.sRGB24read').
newtype HexColor = HexColor Text
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data CustomAttr = CustomAttr
  { name :: String
  , fg :: Maybe HexColor
  , bg :: Maybe HexColor
  , style :: Maybe (Set StyleFlag)
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON CustomAttr where
  toJSON =
    genericToJSON
      defaultOptions
        { omitNothingFields = True
        }
