{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Rendering attributes (/i.e./ foreground and background colors,
-- styles, /etc./) used by the Swarm TUI.
--
-- We export constants only for those we use in the Haskell code
-- and not those used in the world map, to avoid abusing attributes.
-- For example using the robot attribute to highlight some text.
--
-- The few attributes that we use for drawing the logo are an exception.
module Swarm.TUI.View.Attribute.Attr (
  swarmAttrMap,
  worldAttributeNames,
  worldPrefix,
  meterAttributeNames,
  messageAttributeNames,
  toAttrName,
  getWorldAttrName,

  -- ** Terrain attributes
  dirtAttr,
  grassAttr,
  stoneAttr,
  waterAttr,
  iceAttr,

  -- ** Common attributes
  entityAttr,
  robotAttr,
  rockAttr,
  plantAttr,

  -- ** Swarm TUI Attributes
  highlightAttr,
  notifAttr,
  infoAttr,
  boldAttr,
  italicAttr,
  dimAttr,
  magentaAttr,
  cyanAttr,
  lightCyanAttr,
  yellowAttr,
  blueAttr,
  greenAttr,
  redAttr,
  grayAttr,
  defAttr,
  customEditFocusedAttr,
) where

import Brick
import Brick.Forms ( focusedFormInputAttr, invalidFormInputAttr )
import Brick.Widgets.Dialog
import Brick.Widgets.Edit qualified as E
import Brick.Widgets.List ( listSelectedFocusedAttr )
import Data.Bifunctor (bimap, first)
import Data.Colour.Palette.BrewerSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Swarm.Game.Entity.Cosmetic
import Data.Text (unpack)
import Graphics.Vty qualified as V
import Swarm.Game.Display (Attribute (..))
import Data.Colour.SRGB (RGB (..))
import Swarm.TUI.View.Attribute.Util

toAttrName :: Attribute -> AttrName
toAttrName = \case
  ARobot -> robotAttr
  AEntity -> entityAttr
  AWorld n -> worldPrefix <> attrName (unpack n)
  ATerrain n -> terrainPrefix <> attrName (unpack n)
  ADefault -> defAttr

-- | A mapping from the defined attribute names to TUI attributes.
swarmAttrMap :: AttrMap
swarmAttrMap =
  attrMap
    V.defAttr
    $ NE.toList activityMeterAttributes
      <> NE.toList robotMessageAttributes
      <> NE.toList (NE.map (first getWorldAttrName) worldAttributes)
      <> terrainAttr
      <> [ -- Robot attribute
           (robotAttr, fg V.white `V.withStyle` V.bold)
         , -- UI rendering attributes
           (highlightAttr, fg V.cyan)
         , (invalidFormInputAttr, fg V.red)
         , (focusedFormInputAttr, V.defAttr)
         , (customEditFocusedAttr, V.black `on` V.yellow)
         , (listSelectedFocusedAttr, bg V.blue)
         , (infoAttr, fg (V.rgbColor @Int 100 100 100))
         , (buttonSelectedAttr, bg V.blue)
         , (notifAttr, fg V.yellow `V.withStyle` V.bold)
         , (dimAttr, V.defAttr `V.withStyle` V.dim)
         , (boldAttr, V.defAttr `V.withStyle` V.bold)
         , (italicAttr, V.defAttr `V.withStyle` V.italic)
         , -- Basic colors
           (redAttr, fg V.red)
         , (greenAttr, fg V.green)
         , (blueAttr, fg V.blue)
         , (yellowAttr, fg V.yellow)
         , (cyanAttr, fg V.cyan)
         , (lightCyanAttr, fg (V.rgbColor @Int 200 255 255))
         , (magentaAttr, fg V.magenta)
         , (grayAttr, fg (V.rgbColor @Int 128 128 128))
         , -- Default attribute
           (defAttr, V.defAttr)
         ]

worldPrefix :: AttrName
worldPrefix = attrName "world"

getWorldAttrName :: WorldAttr -> AttrName
getWorldAttrName (WorldAttr n) = worldPrefix <> attrName n

whiteRGB :: RGBColor
whiteRGB = RGB 208 207 204

blueRGB :: RGBColor
blueRGB = RGB 42 123 222

greenRGB :: RGBColor
greenRGB = RGB 38 162 105

brightYellowRGB :: RGBColor
brightYellowRGB = RGB 233 173 12

yellowRGB :: RGBColor
yellowRGB = RGB 162 115 76

entity :: (WorldAttr, V.Attr)
entity = (WorldAttr "entity", fg V.white)

entityAttr :: AttrName
entityAttr = getWorldAttrName $ fst entity

water :: (WorldAttr, HiFiColor)
water = (WorldAttr "water", FgAndBg whiteRGB blueRGB)

waterAttr :: AttrName
waterAttr = getWorldAttrName $ fst water

rock :: (WorldAttr, HiFiColor)
rock = (WorldAttr "rock", FgOnly $ RGB 80 80 80)

rockAttr :: AttrName
rockAttr = getWorldAttrName $ fst rock

plant :: (WorldAttr, HiFiColor)
plant = (WorldAttr "plant", FgOnly greenRGB)

plantAttr :: AttrName
plantAttr = getWorldAttrName $ fst rock

-- | Colors of entities in the world.
worldAttributes :: NonEmpty (WorldAttr, V.Attr)
worldAttributes =
  entity
    :| water
    : rock
    : plant
    : map
      (bimap WorldAttr FgOnly)
      [ ("device", brightYellowRGB)
      , ("wood", RGB 139 69 19)
      , ("flower", RGB 200 0 200)
      , ("rubber", RGB 245 224 179)
      , ("copper", yellowRGB)
      , ("copper'", RGB 78 117 102)
      , ("iron", RGB 97 102 106)
      , ("iron'", RGB 183 65 14)
      , ("quartz", whiteRGB)
      , ("silver", RGB 192 192 192)
      , ("gold", RGB 255 215 0)
      , ("snow", whiteRGB)
      , ("sand", RGB 194 178 128)
      , ("fire", RGB 246 97 81)
      , ("red", RGB 192 28 40)
      , ("green", greenRGB)
      , ("blue", blueRGB)
      ]

worldAttributeNames :: Set WorldAttr
worldAttributeNames = Set.fromList $ NE.toList $ NE.map fst worldAttributes

robotMessagePrefix :: AttrName
robotMessagePrefix = attrName "robotMessage"

robotMessageAttributes :: NonEmpty (AttrName, V.Attr)
robotMessageAttributes =
  NE.zip indices $ fromMaybe (pure $ fg V.white) $ NE.nonEmpty brewers
 where
  indices = NE.map ((robotMessagePrefix <>) . attrName . show) $ (0 :: Int) :| [1 ..]
  brewers = map (fg . kolorToAttrColor) $ brewerSet Set3 12

messageAttributeNames :: NonEmpty AttrName
messageAttributeNames = NE.map fst robotMessageAttributes

activityMeterPrefix :: AttrName
activityMeterPrefix = attrName "activityMeter"

activityMeterAttributes :: NonEmpty (AttrName, V.Attr)
activityMeterAttributes =
  NE.zip indices $ fromMaybe (pure $ bg V.black) $ NE.nonEmpty brewers
 where
  indices = NE.map ((activityMeterPrefix <>) . attrName . show) $ (0 :: Int) :| [1 ..]
  brewers = map bgWithAutoForeground $ reverse $ brewerSet RdYlGn 7

meterAttributeNames :: NonEmpty AttrName
meterAttributeNames = NE.map fst activityMeterAttributes

terrainPrefix :: AttrName
terrainPrefix = attrName "terrain"

terrainAttr :: [(AttrName, V.Attr)]
terrainAttr =
  [ (dirtAttr, fg (V.rgbColor @Int 165 42 42))
  , (grassAttr, fg (V.rgbColor @Int 0 32 0)) -- dark green
  , (stoneAttr, fg (V.rgbColor @Int 32 32 32))
  , (iceAttr, bg V.white)
  ]

-- | The default robot attribute.
robotAttr :: AttrName
robotAttr = attrName "robot"

dirtAttr, grassAttr, stoneAttr, iceAttr :: AttrName
dirtAttr = terrainPrefix <> attrName "dirt"
grassAttr = terrainPrefix <> attrName "grass"
stoneAttr = terrainPrefix <> attrName "stone"
iceAttr = terrainPrefix <> attrName "ice"

-- | Some defined attribute names used in the Swarm TUI.
highlightAttr
  , notifAttr
  , infoAttr
  , boldAttr
  , italicAttr
  , dimAttr
  , defAttr ::
    AttrName
highlightAttr = attrName "highlight"
notifAttr = attrName "notif"
infoAttr = attrName "info"
boldAttr = attrName "bold"
italicAttr = attrName "italics"
dimAttr = attrName "dim"
defAttr = attrName "def"

customEditFocusedAttr :: AttrName
customEditFocusedAttr = attrName "custom" <> E.editFocusedAttr

-- | Some basic colors used in TUI.
redAttr, greenAttr, blueAttr, yellowAttr, cyanAttr, lightCyanAttr, magentaAttr, grayAttr :: AttrName
redAttr = attrName "red"
greenAttr = attrName "green"
blueAttr = attrName "blue"
yellowAttr = attrName "yellow"
cyanAttr = attrName "cyan"
lightCyanAttr = attrName "lightCyan"
magentaAttr = attrName "magenta"
grayAttr = attrName "gray"
