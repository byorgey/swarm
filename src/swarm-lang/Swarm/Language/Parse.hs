{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Parser for the Swarm language.  Note, you probably don't want to
-- use this directly, unless there is a good reason to parse a term
-- without also type checking it; use
-- 'Swarm.Language.Pipeline.processTerm' instead, which parses,
-- typechecks, elaborates, and capability checks a term all at once.
module Swarm.Language.Parse (
  parseTerm,
  binOps,
  unOps,

  -- * Utility functions
  readTerm,
  readTerm',
  showShortError,
  showErrorPos,
  getLocRange,
) where

import Control.Lens (view, (^.))
import Control.Monad (guard)
import Control.Monad.Combinators.Expr
import Control.Monad.Reader (ask)
import Data.Bifunctor
import Data.Foldable (asum)
import Data.List (foldl', nub)
import Data.List.NonEmpty qualified (head)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq)
import Data.Set qualified as S
import Data.Set.Lens (setOf)
import Data.Text (Text, index)
import Data.Text qualified as T
import Swarm.Language.Parser.Core
import Swarm.Language.Parser.Lex
import Swarm.Language.Parser.Record (parseRecord)
import Swarm.Language.Parser.Types
import Swarm.Language.Syntax
import Swarm.Language.Types
import Swarm.Util.Parse (fullyMaybe)
import Text.Megaparsec hiding (runParser)
import Text.Megaparsec.Char
import Text.Megaparsec.Pos qualified as Pos
import Witch

-- Imports for doctests (cabal-docspec needs this)

-- $setup
-- >>> import qualified Data.Map.Strict as Map

--------------------------------------------------
-- Parser

parseDirection :: Parser Direction
parseDirection = asum (map alternative allDirs) <?> "direction constant"
 where
  alternative d = d <$ (reserved . directionSyntax) d

-- | Parse Const as reserved words (e.g. @Fail <$ reserved "fail"@)
parseConst :: Parser Const
parseConst = asum (map alternative consts) <?> "built-in user function"
 where
  consts = filter isUserFunc allConst
  alternative c = c <$ reserved (syntax $ constInfo c)

-- | Parse an atomic term, optionally trailed by record projections like @t.x.y.z@.
--   Record projection binds more tightly than function application.
parseTermAtom :: Parser Syntax
parseTermAtom = do
  s1 <- parseTermAtom2
  ps <- many (symbol "." *> parseLocG identifier)
  return $ foldl' (\(Syntax l1 t) (l2, x) -> Syntax (l1 <> l2) (TProj t x)) s1 ps

-- | Parse an atomic term.
parseTermAtom2 :: Parser Syntax
parseTermAtom2 =
  parseLoc
    ( TUnit <$ symbol "()"
        <|> TConst <$> parseConst
        <|> TVar <$> identifier
        <|> TDir <$> parseDirection
        <|> TInt <$> integer
        <|> TText <$> textLiteral
        <|> TBool <$> ((True <$ reserved "true") <|> (False <$ reserved "false"))
        <|> reserved "require"
          *> ( ( TRequireDevice
                  <$> (textLiteral <?> "device name in double quotes")
               )
                <|> ( (TRequire . fromIntegral <$> integer)
                        <*> (textLiteral <?> "entity name in double quotes")
                    )
             )
        <|> uncurry SRequirements <$> (reserved "requirements" *> match parseTerm)
        <|> SLam
          <$> (symbol "\\" *> locIdentifier)
          <*> optional (symbol ":" *> parseType)
          <*> (symbol "." *> parseTerm)
        <|> sLet
          <$> (reserved "let" *> locIdentifier)
          <*> optional (symbol ":" *> parsePolytype)
          <*> (symbol "=" *> parseTerm)
          <*> (reserved "in" *> parseTerm)
        <|> sDef
          <$> (reserved "def" *> locIdentifier)
          <*> optional (symbol ":" *> parsePolytype)
          <*> (symbol "=" *> parseTerm <* reserved "end")
        <|> SRcd <$> brackets (parseRecord (optional (symbol "=" *> parseTerm)))
        <|> parens (view sTerm . mkTuple <$> (parseTerm `sepBy` symbol ","))
    )
    -- Potential syntax for explicitly requesting memoized delay.
    -- Perhaps we will not need this in the end; see the discussion at
    -- https://github.com/swarm-game/swarm/issues/150 .
    -- <|> parseLoc (TDelay SimpleDelay (TConst Noop) <$ try (symbol "{{" *> symbol "}}"))
    -- <|> parseLoc (SDelay MemoizedDelay <$> dbraces parseTerm)

    <|> parseLoc (TDelay SimpleDelay (TConst Noop) <$ try (symbol "{" *> symbol "}"))
    <|> parseLoc (SDelay SimpleDelay <$> braces parseTerm)
    <|> parseLoc (ask >>= (guard . (== AllowAntiquoting)) >> parseAntiquotation)

-- | Construct an 'SLet', automatically filling in the Boolean field
--   indicating whether it is recursive.
sLet :: LocVar -> Maybe Polytype -> Syntax -> Syntax -> Term
sLet x ty t1 = SLet (lvVar x `S.member` setOf freeVarsV t1) x ty t1

-- | Construct an 'SDef', automatically filling in the Boolean field
--   indicating whether it is recursive.
sDef :: LocVar -> Maybe Polytype -> Syntax -> Term
sDef x ty t = SDef (lvVar x `S.member` setOf freeVarsV t) x ty t

parseAntiquotation :: Parser Term
parseAntiquotation =
  TAntiText <$> (lexeme . try) (symbol "$str:" *> identifier)
    <|> TAntiInt <$> (lexeme . try) (symbol "$int:" *> identifier)

-- | Parse a Swarm language term.
parseTerm :: Parser Syntax
parseTerm = sepEndBy1 parseStmt (symbol ";") >>= mkBindChain

mkBindChain :: [Stmt] -> Parser Syntax
mkBindChain stmts = case last stmts of
  Binder x _ -> return $ foldr mkBind (STerm (TApp (TConst Return) (TVar (lvVar x)))) stmts
  BareTerm t -> return $ foldr mkBind t (init stmts)
 where
  mkBind (BareTerm t1) t2 = loc Nothing t1 t2 $ SBind Nothing t1 t2
  mkBind (Binder x t1) t2 = loc (Just x) t1 t2 $ SBind (Just x) t1 t2
  loc mx a b = Syntax $ maybe NoLoc lvSrcLoc mx <> (a ^. sLoc) <> (b ^. sLoc)

data Stmt
  = BareTerm Syntax
  | Binder LocVar Syntax
  deriving (Show)

parseStmt :: Parser Stmt
parseStmt =
  mkStmt <$> optional (try (locIdentifier <* symbol "<-")) <*> parseExpr

mkStmt :: Maybe LocVar -> Syntax -> Stmt
mkStmt Nothing = BareTerm
mkStmt (Just x) = Binder x

-- | When semicolons are missing between definitions, for example:
--     def a = 1 end def b = 2 end def c = 3 end
--   The makeExprParser produces:
--     App (App (TDef a) (TDef b)) (TDef x)
--   This function fix that by converting the Apps into Binds, so that it results in:
--     Bind a (Bind b (Bind c))
fixDefMissingSemis :: Syntax -> Syntax
fixDefMissingSemis term =
  case nestedDefs term [] of
    [] -> term
    defs -> foldr1 mkBind defs
 where
  mkBind t1 t2 = Syntax ((t1 ^. sLoc) <> (t2 ^. sLoc)) $ SBind Nothing t1 t2
  nestedDefs term' acc = case term' of
    def@(Syntax _ SDef {}) -> def : acc
    (Syntax _ (SApp nestedTerm def@(Syntax _ SDef {}))) -> nestedDefs nestedTerm (def : acc)
    -- Otherwise returns an empty list to keep the term unchanged
    _ -> []

parseExpr :: Parser Syntax
parseExpr =
  parseLoc $ ascribe <$> parseExpr' <*> optional (symbol ":" *> parsePolytype)
 where
  ascribe :: Syntax -> Maybe Polytype -> Term
  ascribe s Nothing = s ^. sTerm
  ascribe s (Just ty) = SAnnotate s ty

parseExpr' :: Parser Syntax
parseExpr' = fixDefMissingSemis <$> makeExprParser parseTermAtom table
 where
  table = snd <$> Map.toDescList tableMap
  tableMap =
    Map.unionsWith
      (++)
      [ Map.singleton 9 [InfixL (exprLoc2 $ SApp <$ string "")]
      , binOps
      , unOps
      ]

  -- add location for ExprParser by combining all
  exprLoc2 :: Parser (Syntax -> Syntax -> Term) -> Parser (Syntax -> Syntax -> Syntax)
  exprLoc2 p = do
    (l, f) <- parseLocG p
    pure $ \s1 s2 -> Syntax (l <> (s1 ^. sLoc) <> (s2 ^. sLoc)) $ f s1 s2

-- | Precedences and parsers of binary operators.
--
-- >>> Map.map length binOps
-- fromList [(0,1),(2,1),(3,1),(4,6),(6,3),(7,2),(8,1)]
binOps :: Map.Map Int [Operator Parser Syntax]
binOps = Map.unionsWith (++) $ mapMaybe binOpToTuple allConst
 where
  binOpToTuple c = do
    let ci = constInfo c
    ConstMBinOp assoc <- pure (constMeta ci)
    let assI = case assoc of
          L -> InfixL
          N -> InfixN
          R -> InfixR
    pure $
      Map.singleton
        (fixity ci)
        [assI (mkOp c <$ operatorString (syntax ci))]

-- | Precedences and parsers of unary operators (currently only 'Neg').
--
-- >>> Map.map length unOps
-- fromList [(7,1)]
unOps :: Map.Map Int [Operator Parser Syntax]
unOps = Map.unionsWith (++) $ mapMaybe unOpToTuple allConst
 where
  unOpToTuple c = do
    let ci = constInfo c
    ConstMUnOp assoc <- pure (constMeta ci)
    let assI = case assoc of
          P -> Prefix
          S -> Postfix
    pure $
      Map.singleton
        (fixity ci)
        [assI (exprLoc1 $ SApp (noLoc $ TConst c) <$ operatorString (syntax ci))]

  -- combine location for ExprParser
  exprLoc1 :: Parser (Syntax -> Term) -> Parser (Syntax -> Syntax)
  exprLoc1 p = do
    (l, f) <- parseLocG p
    pure $ \s -> Syntax (l <> s ^. sLoc) $ f s

operatorString :: Text -> Parser Text
operatorString n = (lexeme . try) (string n <* notFollowedBy operatorSymbol)

operatorSymbol :: Parser Text
operatorSymbol = T.singleton <$> oneOf opChars
 where
  isOp = \case { ConstMFunc {} -> False; _ -> True } . constMeta
  opChars = nub . concatMap (from . syntax) . filter isOp $ map constInfo allConst

--------------------------------------------------
-- Utilities

-- | Parse some input 'Text' completely as a 'Term', consuming leading
--   whitespace and ensuring the parsing extends all the way to the
--   end of the input 'Text'.  Returns either the resulting 'Term' (or
--   'Nothing' if the input was only whitespace) or a pretty-printed
--   parse error message.
readTerm :: Text -> Either Text (Maybe Syntax)
readTerm = bimap (from . errorBundlePretty) fst . runParser (fullyMaybe sc parseTerm)

-- | A lower-level `readTerm` which returns the megaparsec bundle error
--   for precise error reporting, as well as the parsed comments.
readTerm' :: Text -> Either ParserError (Maybe Syntax, Seq Comment)
readTerm' = runParser (fullyMaybe sc parseTerm)

-- | A utility for converting a ParserError into a one line message:
--   @<line-nr>: <error-msg>@
showShortError :: ParserError -> String
showShortError pe = show (line + 1) <> ": " <> from msg
 where
  ((line, _), _, msg) = showErrorPos pe

-- | A utility for converting a ParseError into a range and error message.
showErrorPos :: ParserError -> ((Int, Int), (Int, Int), Text)
showErrorPos (ParseErrorBundle errs sourcePS) = (minusOne start, minusOne end, from msg)
 where
  -- convert megaparsec source pos to starts at 0
  minusOne (x, y) = (x - 1, y - 1)

  -- get the first error position (ps) and line content (str)
  err = Data.List.NonEmpty.head errs
  offset = case err of
    TrivialError x _ _ -> x
    FancyError x _ -> x
  (str, ps) = reachOffset offset sourcePS
  msg = parseErrorTextPretty err

  -- extract the error starting position
  start@(line, col) = getLineCol ps

  -- compute the ending position based on the word at starting position
  wordlength = case break (== ' ') . drop col <$> str of
    Just (word, _) -> length word + 1
    _ -> 0
  end = (line, col + wordlength)

getLineCol :: PosState a -> (Int, Int)
getLineCol ps = (line, col)
 where
  line = unPos $ sourceLine $ pstateSourcePos ps
  col = unPos $ sourceColumn $ pstateSourcePos ps

-- | A utility for converting a SrcLoc into a range
getLocRange :: Text -> (Int, Int) -> ((Int, Int), (Int, Int))
getLocRange code (locStart, locEnd) = (start, end)
 where
  start = getLocPos locStart
  end = getLocPos (dropWhiteSpace locEnd)

  -- remove trailing whitespace that got included by the lexer
  dropWhiteSpace offset
    | isWhiteSpace offset = dropWhiteSpace (offset - 1)
    | otherwise = offset
  isWhiteSpace offset =
    -- Megaparsec offset needs to be (-1) to start at 0
    Data.Text.index code (offset - 1) `elem` [' ', '\n', '\r', '\t']

  -- using megaparsec offset facility, compute the line/col
  getLocPos offset =
    let sourcePS =
          PosState
            { pstateInput = code
            , pstateOffset = 0
            , pstateSourcePos = Pos.initialPos ""
            , pstateTabWidth = Pos.defaultTabWidth
            , pstateLinePrefix = ""
            }
        (_, ps) = reachOffset offset sourcePS
     in getLineCol ps
