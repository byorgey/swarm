{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Pretty-printing failure messages
module Swarm.Game.Failure.Render where

import Data.Char (toLower)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml (prettyPrintParseException)
import Swarm.Game.Failure
import Swarm.Util (quote)
import Text.Megaparsec (errorBundlePretty)

tShowLow :: Show a => a -> Text
tShowLow = T.pack . map toLower . show

tShow :: Show a => a -> Text
tShow = T.pack . show

prettyLoadingFailure :: LoadingFailure -> Text
prettyLoadingFailure = \case
  DoesNotExist e -> "The " <> tShowLow e <> " is missing!"
  EntryNot e -> "The entry is not a " <> tShowLow e <> "!"
  CanNotParseYaml p -> "Parse failure:\n" <> T.pack (indent 8 $ prettyPrintParseException p)
  CanNotParseMegaparsec p -> "Parse failure:\n" <> T.pack (errorBundlePretty p)
  DoesNotTypecheck t -> "Error during typechecking:\n" <> t
  CustomMessage m -> m
 where
  indent n = unlines . map (replicate n ' ' ++) . lines

prettyFailure :: SystemFailure -> Text
prettyFailure = \case
  AssetNotLoaded a fp l ->
    T.unwords ["Failed to acquire", tShowLow a, tShow fp, "from path", quote $ T.pack fp] <> ": " <> prettyLoadingFailure l
