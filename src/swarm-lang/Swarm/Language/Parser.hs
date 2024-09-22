{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Parser for the Swarm language.  Note, you probably don't want to
-- use this directly, unless there is a good reason to parse a term
-- without also type checking it; use
-- 'Swarm.Language.Pipeline.processTerm' instead, which parses,
-- typechecks, elaborates, and capability checks a term all at once.
module Swarm.Language.Parser (
  readTerm,
  readNonemptyTerm,
  readTerm',
) where

import Control.Monad ((>=>))
import Data.Bifunctor (first, second)
import Data.Either.Extra (maybeToEither)
import Data.Sequence (Seq)
import Data.Text (Text)
import Swarm.Language.Parser.Comment (populateComments)
import Swarm.Language.Parser.Core (ParserConfig, ParserError, defaultParserConfig, runParser')
import Swarm.Language.Parser.Lex (sc)
import Swarm.Language.Parser.Term (parseTerm)
import Swarm.Language.Parser.Util (fullyMaybe)
import Swarm.Language.Syntax (Comment, Syntax)
import Text.Megaparsec.Error (errorBundlePretty)
import Witch (from)

-- | Parse some input 'Text' completely as a 'Term', consuming leading
--   whitespace and ensuring the parsing extends all the way to the
--   end of the input 'Text'.  Returns an error if the term was only
--   whitespace.
readNonemptyTerm :: Text -> Either Text Syntax
readNonemptyTerm = readTerm >=> maybeToEither "Empty term"

-- | Parse some input 'Text' completely as a 'Term', consuming leading
--   whitespace and ensuring the parsing extends all the way to the
--   end of the input 'Text'.  Returns either the resulting 'Term' (or
--   'Nothing' if the input was only whitespace) or a pretty-printed
--   parse error message.
readTerm :: Text -> Either Text (Maybe Syntax)
readTerm = first (from . errorBundlePretty) . readTerm' defaultParserConfig

-- | A lower-level `readTerm` which allows configuring the parser and
--   returns the megaparsec bundle error for precise error reporting.
readTerm' :: ParserConfig -> Text -> Either ParserError (Maybe Syntax)
readTerm' cfg = second handleComments . runParser' cfg (fullyMaybe sc parseTerm)
 where
  handleComments :: (Maybe Syntax, Seq Comment) -> Maybe Syntax
  handleComments (s, cs) = populateComments cs <$> s
