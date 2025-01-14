{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Orphan Hashable instances needed to derive Hashable Display

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: TUI rendering of entities
--
-- Utilities for describing how to display in-game entities in the TUI.
module Swarm.Game.Display (
  -- * The display record
  Priority,
  Attribute (..),
  readAttribute,
  Display,
  ChildInheritance (..),
  BackgroundSource (..),

  -- ** Fields
  defaultChar,
  orientationMap,
  curOrientation,
  boundaryOverride,
  displayAttr,
  displayPriority,
  invisible,
  childInheritance,
  backgroundSources,
  _backgroundSources,

  -- ** Rendering
  displayChar,
  hidden,

  -- ** Neighbor-based boundary rendering
  getBoundaryDisplay,

  -- ** Construction
  defaultTerrainDisplay,
  defaultEntityDisplay,
  defaultRobotDisplay,
) where

import Control.Applicative ((<|>))
import Control.Lens hiding (Const, from, (.=))
import Control.Monad (when)
import Data.Hashable (Hashable)
import Data.List.Extra (enumerate)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml
import GHC.Generics (Generic)
import Graphics.Text.Width
import Swarm.Game.Entity.Cosmetic
import Swarm.Language.Syntax.Direction (AbsoluteDir (..), Direction (..))
import Swarm.Util (applyWhen, maxOn, quote)
import Swarm.Util.Lens (makeLensesNoSigs)
import Swarm.Util.Yaml (FromJSONE (..), With (runE), getE, liftE, withObjectE)

-- | Display priority.  Entities with higher priority will be drawn on
--   top of entities with lower priority.
type Priority = Int

-- | An internal attribute name.
data Attribute = ADefault | ARobot | AEntity | AWorld Text
  deriving (Eq, Ord, Show, Generic, Hashable)

readAttribute :: Text -> Attribute
readAttribute = \case
  "robot" -> ARobot
  "entity" -> AEntity
  "default" -> ADefault
  w -> AWorld w

instance FromJSON Attribute where
  parseJSON = withText "attribute" $ pure . readAttribute

instance ToJSON Attribute where
  toJSON = \case
    ADefault -> String "default"
    ARobot -> String "robot"
    AEntity -> String "entity"
    AWorld w -> String w

data ChildInheritance
  = Invisible
  | Inherit
  | DefaultDisplay
  deriving (Eq, Ord, Show, Generic, Hashable)

data BackgroundSource a = BackgroundSource
  { terrainBgAttr :: Maybe a
  , entityBgAttr :: Maybe a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic, Hashable)

instance Semigroup (BackgroundSource a) where
  BackgroundSource x1 y1 <> BackgroundSource x2 y2 =
    BackgroundSource (x1 <|> x2) (y1 <|> y2)

instance Monoid (BackgroundSource a) where
  mempty = BackgroundSource Nothing Nothing

-- | A record explaining how to display an entity in the TUI.
data Display = Display
  { _defaultChar :: Char
  , _orientationMap :: Map AbsoluteDir Char
  , _curOrientation :: Maybe Direction
  , _boundaryOverride :: Maybe Char
  , _displayAttr :: Attribute
  , _displayPriority :: Priority
  , _invisible :: Bool
  , _childInheritance :: ChildInheritance
  , _backgroundSources :: BackgroundSource WorldAttr
  }
  deriving (Eq, Show, Generic, Hashable)

instance Semigroup Display where
  d1 <> d2
    | _invisible d1 = d2
    | _invisible d2 = d1
    | otherwise =
        (maxOn _displayPriority d1 d2)
          { _backgroundSources = _backgroundSources d1 <> _backgroundSources d2
          }

makeLensesNoSigs ''Display

-- | The default character to use for display.
defaultChar :: Lens' Display Char

-- | For robots or other entities that have an orientation, this map
--   optionally associates different display characters with
--   different orientations.  If an orientation is not in the map,
--   the 'defaultChar' will be used.
orientationMap :: Lens' Display (Map AbsoluteDir Char)

-- | The display caches the current orientation of the entity, so we
--   know which character to use from the orientation map.
curOrientation :: Lens' Display (Maybe Direction)

-- | The display character to substitute when neighbor boundaries are present
boundaryOverride :: Lens' Display (Maybe Char)

-- | The attribute to use for display.
displayAttr :: Lens' Display Attribute

-- | This entity's display priority. Higher priorities are drawn
--   on top of lower.
displayPriority :: Lens' Display Priority

-- | Whether the entity is currently invisible.
invisible :: Lens' Display Bool

-- | For robots, whether children of this inherit the parent's display
childInheritance :: Lens' Display ChildInheritance

-- | Background color
backgroundSources :: Lens' Display (BackgroundSource WorldAttr)

instance FromJSON Display where
  parseJSON v = runE (parseJSONE v) (defaultEntityDisplay ' ')

instance FromJSONE Display Display where
  parseJSONE = withObjectE "Display" $ \v -> do
    defD <- getE
    mc <- liftE $ v .:? "char"

    let c = fromMaybe (defD ^. defaultChar) mc
    validateChar c

    let dOM = if isJust mc then mempty else defD ^. orientationMap
    mapM_ validateChar $ M.elems dOM

    liftE $ do
      let _defaultChar = c
          _boundaryOverride = Nothing
          _backgroundSources = mempty
      _orientationMap <- v .:? "orientationMap" .!= dOM
      _curOrientation <- v .:? "curOrientation" .!= (defD ^. curOrientation)
      _displayAttr <- (v .:? "attr") .!= (defD ^. displayAttr)
      _displayPriority <- v .:? "priority" .!= (defD ^. displayPriority)
      _invisible <- v .:? "invisible" .!= (defD ^. invisible)
      let _childInheritance = Inherit
      pure Display {..}
   where
    validateChar c =
      when (charWidth > 1)
        . fail
        . T.unpack
        $ T.unwords
          [ "Character"
          , quote $ T.singleton c
          , "is too wide:"
          , T.pack $ show charWidth
          ]
     where
      charWidth = safeWcwidth c

instance ToJSON Display where
  toJSON d =
    object $
      [ "char" .= (d ^. defaultChar)
      , "attr" .= (d ^. displayAttr)
      , "priority" .= (d ^. displayPriority)
      ]
        ++ ["orientationMap" .= (d ^. orientationMap) | not (M.null (d ^. orientationMap))]
        ++ ["invisible" .= (d ^. invisible) | d ^. invisible]

-- | Look up the character that should be used for a display.
displayChar :: Display -> Char
displayChar disp =
  fromMaybe (disp ^. defaultChar) $
    disp ^. boundaryOverride <|> do
      DAbsolute d <- disp ^. curOrientation
      M.lookup d (disp ^. orientationMap)

-- | Modify a display to use a @?@ character for entities that are
--   hidden/unknown.
hidden :: Display -> Display
hidden = (defaultChar .~ '?') . (curOrientation .~ Nothing)

-- | The default way to display some terrain using the given character
--   and attribute, with priority 0.
defaultTerrainDisplay :: Attribute -> Display
defaultTerrainDisplay attr =
  defaultEntityDisplay ' '
    & displayPriority .~ 0
    & displayAttr .~ attr

-- | Construct a default display for an entity that uses only a single
--   display character, the default entity attribute, and priority 1.
defaultEntityDisplay :: Char -> Display
defaultEntityDisplay c =
  Display
    { _defaultChar = c
    , _orientationMap = M.empty
    , _curOrientation = Nothing
    , _boundaryOverride = Nothing
    , _displayAttr = AEntity
    , _displayPriority = 1
    , _invisible = False
    , _childInheritance = Inherit
    , _backgroundSources = mempty
    }

-- | Construct a default robot display for a given orientation, with
--   display characters @"X^>v<"@, the default robot attribute, and
--   priority 10.
--
--   Note that the 'defaultChar' is used for direction 'DDown'
--   and is overridden for the special base robot.
defaultRobotDisplay :: Display
defaultRobotDisplay =
  Display
    { _defaultChar = 'X'
    , _orientationMap =
        M.fromList
          [ (DEast, '>')
          , (DWest, '<')
          , (DSouth, 'v')
          , (DNorth, '^')
          ]
    , _boundaryOverride = Nothing
    , _curOrientation = Nothing
    , _displayAttr = ARobot
    , _displayPriority = 10
    , _invisible = False
    , _childInheritance = Inherit
    , _backgroundSources = mempty
    }

instance Monoid Display where
  mempty = defaultEntityDisplay ' ' & invisible .~ True

-- * Boundary rendering

-- | This type is isomorphic to 'Bool' but
-- is more compact for readability of the
-- 'glyphForNeighbors' cases.
data Presence
  = -- | present
    X
  | -- | absent
    O

emptyNeighbors :: Neighbors Presence
emptyNeighbors = Neighbors O O O O

data Neighbors a = Neighbors
  { e :: a
  , w :: a
  , n :: a
  , s :: a
  }

computeNeighborPresence :: (AbsoluteDir -> Bool) -> Neighbors Presence
computeNeighborPresence checkPresence =
  foldr assignPresence emptyNeighbors enumerate
 where
  assignPresence d = applyWhen (checkPresence d) $ setNeighbor d X

setNeighbor :: AbsoluteDir -> a -> Neighbors a -> Neighbors a
setNeighbor DNorth x y = y {n = x}
setNeighbor DSouth x y = y {s = x}
setNeighbor DEast x y = y {e = x}
setNeighbor DWest x y = y {w = x}

-- | For a center cell that itself is a boundary,
-- determine a glyph override for rendering, given certain
-- neighbor combinations.
glyphForNeighbors :: Neighbors Presence -> Maybe Char
glyphForNeighbors = \case
  Neighbors {e = O, w = O, n = O, s = O} -> Nothing
  Neighbors {e = X, w = O, n = O, s = O} -> Just '╶'
  Neighbors {e = O, w = X, n = O, s = O} -> Just '╴'
  Neighbors {e = X, w = X, n = O, s = O} -> Just '─'
  Neighbors {e = O, w = O, n = X, s = O} -> Just '╵'
  Neighbors {e = O, w = O, n = O, s = X} -> Just '╷'
  Neighbors {e = O, w = O, n = X, s = X} -> Just '│'
  Neighbors {e = X, w = O, n = X, s = O} -> Just '└'
  Neighbors {e = X, w = O, n = O, s = X} -> Just '┌'
  Neighbors {e = O, w = X, n = X, s = O} -> Just '┘'
  Neighbors {e = O, w = X, n = O, s = X} -> Just '┐'
  Neighbors {e = X, w = X, n = X, s = O} -> Just '┴'
  Neighbors {e = X, w = X, n = O, s = X} -> Just '┬'
  Neighbors {e = X, w = O, n = X, s = X} -> Just '├'
  Neighbors {e = O, w = X, n = X, s = X} -> Just '┤'
  Neighbors {e = X, w = X, n = X, s = X} -> Just '┼'

getBoundaryDisplay :: (AbsoluteDir -> Bool) -> Maybe Char
getBoundaryDisplay = glyphForNeighbors . computeNeighborPresence
