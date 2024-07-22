{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Token lexing and comment preservation for the Swarm language.
module Swarm.Language.Parser.Lex (
  -- * Parsing with source locations
  parseLoc,
  parseLocG,

  -- * Whitespace + comments
  getCommentSituation,
  lineComment,
  blockComment,
  sc,

  -- * Tokens

  -- ** Lexemes
  lexeme,

  -- ** Specific token types
  symbol,
  operator,
  reservedWords,
  reservedCS,
  reserved,
  IdentifierType (..),
  locIdentifier,
  locTmVar,
  locTyName,
  identifier,
  tyVar,
  tyName,
  tmVar,
  textLiteral,
  integer,

  -- ** Combinators
  braces,
  parens,
  brackets,
) where

import Control.Lens (use, view, (%=), (.=))
import Control.Monad (void)
import Data.Char (isLower, isUpper)
import Data.Containers.ListUtils (nubOrd)
import Data.List.Extra (enumerate)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Language.Parser.Core
import Swarm.Language.Syntax
import Swarm.Language.Syntax.Direction
import Swarm.Language.Types (baseTyName)
import Swarm.Util (failT, squote)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Witch (from, into)

------------------------------------------------------------
-- Parsing with source locations

-- | Add 'SrcLoc' to a parser
parseLocG :: Parser a -> Parser (SrcLoc, a)
parseLocG pa = do
  start <- getOffset
  a <- pa
  end <- getOffset
  pure (SrcLoc start end, a)

-- | Add 'SrcLoc' to a 'Term' parser
parseLoc :: Parser Term -> Parser Syntax
parseLoc pterm = uncurry Syntax <$> parseLocG pterm

------------------------------------------------------------
-- Whitespace

-- Approach for preserving comments taken from https://www.reddit.com/r/haskell/comments/ni4gpm/comment/gz0ipmp/

-- | If we see a comment starting now, is it the first non-whitespace
--   thing on the current line so far, or were there other
--   non-whitespace tokens previously?
getCommentSituation :: Parser CommentSituation
getCommentSituation = do
  fl <- use freshLine
  return $ if fl then StandaloneComment else SuffixComment

-- | Parse a line comment, while appending it out-of-band to the list of
--   comments saved in the custom state.
lineComment :: Text -> Parser ()
lineComment start = do
  cs <- getCommentSituation
  (loc, t) <- parseLocG $ do
    string start *> takeWhileP (Just "character") (/= '\n')
  comments %= (Seq.|> Comment loc LineComment cs t)

-- | Parse a block comment, while appending it out-of-band to the list of
--   comments saved in the custom state.
blockComment :: Text -> Text -> Parser ()
blockComment start end = do
  cs <- getCommentSituation
  (loc, t) <- parseLocG $ do
    void $ string start
    manyTill anySingle (string end)
  comments %= (Seq.|> Comment loc BlockComment cs (into @Text t))

-- | Skip spaces and comments.
sc :: Parser ()
sc =
  -- Typically we would use L.space here, but we have to inline its
  -- definition and use our own slight variant, since we need to treat
  -- end-of-line specially.
  skipMany . choice . map hidden $
    [ hspace1
    , eol *> (freshLine .= True) -- If we see a newline, reset freshLine to True.
    , lineComment "//"
    , blockComment "/*" "*/"
    ]

------------------------------------------------------------
-- Tokens

-- | In general, we follow the convention that every token parser
--   assumes no leading whitespace and consumes all trailing
--   whitespace.  Concretely, we achieve this by wrapping every token
--   parser using 'lexeme'.
--
--   Also sets freshLine to False every time we see a non-whitespace
--   token.
lexeme :: Parser a -> Parser a
lexeme p = (freshLine .= False) *> L.lexeme sc p

-- | A lexeme consisting of a literal string.
symbol :: Text -> Parser Text
symbol s = (freshLine .= False) *> L.symbol sc s

-- | A lexeme consisting of a specific string, not followed by any other
--   operator character.
operator :: Text -> Parser Text
operator n = (lexeme . try) (string n <* notFollowedBy operatorChar)

-- | Recognize a single character which is one of the characters used
--   by a built-in operator.
operatorChar :: Parser Text
operatorChar = T.singleton <$> oneOf opChars
 where
  isOp = \case { ConstMFunc {} -> False; _ -> True } . constMeta
  opChars = nubOrd . concatMap (from . syntax) . filter isOp $ map constInfo allConst

-- | Names of base types built into the language.
baseTypeNames :: [Text]
baseTypeNames = map baseTyName enumerate

-- | Names of types built into the language.
primitiveTypeNames :: [Text]
primitiveTypeNames = "Cmd" : baseTypeNames

-- | List of keywords built into the language.
keywords :: [Text]
keywords = T.words "let in def tydef end true false forall require requirements rec import"

-- | List of reserved words that cannot be used as variable names.
reservedWords :: Set Text
reservedWords =
  S.fromList $
    map (syntax . constInfo) (filter isUserFunc allConst)
      ++ map directionSyntax allDirs
      ++ primitiveTypeNames
      ++ keywords

-- | Cached version of the reserved words list with everything
--   lowercase, for use in parsing version 0.5 of the language, where
--   types were lowercase instead of uppercase.
lowerReservedWords :: Set Text
lowerReservedWords = S.map T.toLower reservedWords

-- | Parse a reserved word, given a string recognizer (which can
--   /e.g./ be case sensitive or not), making sure it is not a prefix
--   of a longer variable name, and allowing the parser to backtrack
--   if it fails.
reservedGen :: (Text -> Parser a) -> Text -> Parser ()
reservedGen str w = (lexeme . try) $ str w *> notFollowedBy (alphaNumChar <|> char '_')

-- | Parse a case-sensitive reserved word.
reservedCS :: Text -> Parser ()
reservedCS = reservedGen string

-- | Parse a case-insensitive reserved word.
reserved :: Text -> Parser ()
reserved = reservedGen string'

-- | What kind of identifier are we parsing?
data IdentifierType = IDTyVar | IDTyName | IDTmVar
  deriving (Eq, Ord, Show)

-- | Parse an identifier together with its source location info.
locIdentifier :: IdentifierType -> Parser LocVar
locIdentifier idTy = do
  ver <- view languageVersion
  uncurry LV <$> parseLocG ((lexeme . try) (p >>= check ver) <?> "variable name")
 where
  p = (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_' <|> char '\'')
  check ver (into @Text -> t) = case ver of
    SwarmLang0_5
      | T.toLower t `S.member` lowerReservedWords ->
          failT ["reserved word", squote t, "cannot be used as variable name"]
      | otherwise -> return t
    SwarmLangLatest
      | IDTyVar <- idTy
      , T.toTitle t `S.member` reservedWords ->
          failT ["Reserved type name", squote t, "cannot be used as a type variable name; perhaps you meant", squote (T.toTitle t) <> "?"]
      | IDTyName <- idTy
      , t `S.member` reservedWords ->
          failT ["Reserved type name", squote t, "cannot be redefined."]
      | t `S.member` reservedWords || T.toLower t `S.member` reservedWords ->
          failT ["Reserved word", squote t, "cannot be used as a variable name"]
      | IDTyName <- idTy
      , isLower (T.head t) ->
          failT ["Type synonym names must start with an uppercase letter"]
      | IDTyVar <- idTy
      , isUpper (T.head t) ->
          failT ["Type variable names must start with a lowercase letter"]
      | otherwise -> return t

-- | Parse a term variable together with its source location info.
locTmVar :: Parser LocVar
locTmVar = locIdentifier IDTmVar

-- | Parse a user-defined type name together with its source location
--   info.
locTyName :: Parser LocVar
locTyName = locIdentifier IDTyName

-- | Parse an identifier, i.e. any non-reserved string containing
--   alphanumeric characters and underscores, not starting with a
--   digit. The Bool indicates whether we are parsing a type variable.
identifier :: IdentifierType -> Parser Var
identifier = fmap lvVar . locIdentifier

-- | Parse a type variable, which must start with an underscore or
--   lowercase letter and cannot be the lowercase version of a type
--   name.
tyVar :: Parser Var
tyVar = identifier IDTyVar

-- | Parse a (user-defined) type constructor name, which must start
--   with an uppercase letter.
tyName :: Parser Var
tyName = identifier IDTyName

-- | Parse a term variable, which can start in any case and just
--   cannot be the same (case-insensitively) as a lowercase reserved
--   word.
tmVar :: Parser Var
tmVar = identifier IDTmVar

-- | Parse a text literal (including escape sequences) in double quotes.
textLiteral :: Parser Text
textLiteral = into <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))

-- | Parse a positive integer literal token, in decimal, binary,
--   octal, or hexadecimal notation.  Note that negation is handled as
--   a separate operator.
integer :: Parser Integer
integer =
  label "integer literal" $
    lexeme $ do
      n <-
        string "0b"
          *> L.binary
          <|> string "0o"
            *> L.octal
          <|> string "0x"
            *> L.hexadecimal
          <|> L.decimal
      notFollowedBy alphaNumChar
      return n

------------------------------------------------------------
-- Combinators

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")
