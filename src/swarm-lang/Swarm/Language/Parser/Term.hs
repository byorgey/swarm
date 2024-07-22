{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Parser for terms of the Swarm language.
module Swarm.Language.Parser.Term where

import Control.Lens (view, (^.))
import Control.Monad (guard, join, void)
import Control.Monad.Combinators.Expr
import Data.Foldable (Foldable (..), asum)
import Data.Functor (($>))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Data.Set.Lens (setOf)
import Data.Text (Text)
import Swarm.Language.Parser.Core
import Swarm.Language.Parser.Lex
import Swarm.Language.Parser.Record (parseRecord)
import Swarm.Language.Parser.Type
import Swarm.Language.Syntax
import Swarm.Language.Syntax.Direction
import Swarm.Language.Types
import Swarm.Util (failT, findDup)
import Text.Megaparsec hiding (runParser)
import Text.Megaparsec.Char
import Prelude hiding (Foldable (..))

-- Imports for doctests (cabal-docspec needs this)

-- $setup
-- >>> import qualified Data.Map.Strict as M

--------------------------------------------------
-- Parser

parseDirection :: Parser Direction
parseDirection = asum (map alternative allDirs) <?> "direction constant"
 where
  alternative d = d <$ (reserved . directionSyntax) d

-- | Parse Const as reserved words (e.g. @Fail <$ reserved "fail"@)
parseConst :: Parser Const
parseConst = do
  ver <- view languageVersion
  asum (map (alternative ver) consts) <?> "built-in user function"
 where
  consts = filter isUserFunc allConst
  alternative ver c = c <$ reserved (syntax $ constInfo' ver c)

  -- Version 0.6 of the language had a constant named @return@, which
  -- is now renamed to @pure@
  constInfo' SwarmLang0_6 Pure = (constInfo Pure) {syntax = "return"}
  constInfo' _ c = constInfo c

-- | Parse an atomic term, optionally trailed by record projections like @t.x.y.z@.
--   Record projection binds more tightly than function application.
parseTermAtom :: Parser Syntax
parseTermAtom = do
  s1 <- parseTermAtom2
  ps <- many (symbol "." *> parseLocG tmVar)
  return $ foldl' (\(Syntax l1 t) (l2, x) -> Syntax (l1 <> l2) (TProj t x)) s1 ps

-- | Parse an atomic term.
parseTermAtom2 :: Parser Syntax
parseTermAtom2 =
  parseLoc
    ( TUnit <$ symbol "()"
        <|> TConst <$> parseConst
        <|> TVar <$> tmVar
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
          <$> (symbol "\\" *> locTmVar)
          <*> optional (symbol ":" *> parseType)
          <*> (symbol "." *> parseTerm)
        <|> sLet LSLet
          <$> (reserved "let" *> locTmVar)
          <*> optional (symbol ":" *> parsePolytype)
          <*> (symbol "=" *> parseTerm)
          <*> (reserved "in" *> parseTerm)
        <|> sLet LSDef
          <$> (reserved "def" *> locTmVar)
          <*> optional (symbol ":" *> parsePolytype)
          <*> (symbol "=" *> parseTerm <* reserved "end")
          <*> (optional (symbol ";") *> (parseTerm <|> (eof $> sNoop)))
        <|> STydef
          <$> (reserved "tydef" *> locTyName)
          <*> join (bindTydef <$> many tyVar <*> (symbol "=" *> parseType <* reserved "end"))
          <*> pure Nothing
          <*> (optional (symbol ";") *> (parseTerm <|> (eof $> sNoop)))
        <|> SRcd <$> brackets (parseRecord (optional (symbol "=" *> parseTerm)))
        <|> SImportIn
          <$> parseURL
          <*> (optional (void (symbol ";") <|> reserved "in") *> (parseTerm <|> (eof $> sNoop)))
        <|> parens (view sTerm . mkTuple <$> (parseTerm `sepBy` symbol ","))
    )
    <|> parseLoc (TDelay (TConst Noop) <$ try (symbol "{" *> symbol "}"))
    <|> parseLoc (SDelay <$> braces parseTerm)
    <|> parseLoc (view antiquoting >>= (guard . (== AllowAntiquoting)) >> parseAntiquotation)

parseURL :: Parser ImportLocation
parseURL = undefined

-- | Construct an 'SLet', automatically filling in the Boolean field
--   indicating whether it is recursive.
sLet :: LetSyntax -> LocVar -> Maybe RawPolytype -> Syntax -> Syntax -> Term
sLet ls x ty t1 = SLet ls (lvVar x `S.member` setOf freeVarsV t1) x ty Nothing mempty t1

sNoop :: Syntax
sNoop = STerm (TConst Noop)

-- | Create a polytype from a list of variable binders and a type.
--   Ensure that no binder is repeated, and all type variables in the
--   type are present in the list of binders (/i.e./ the type contains
--   no free type variables).
bindTydef :: [Var] -> Type -> Parser Polytype
bindTydef xs ty
  | Just repeated <- findDup xs =
      failT ["Duplicate variable on left-hand side of tydef:", repeated]
  | not (S.null free) =
      failT $
        "Undefined type variable(s) on right-hand side of tydef:" : S.toList free
  | otherwise = return . absQuantify $ mkPoly xs ty
 where
  free = tyVars ty `S.difference` S.fromList xs

parseAntiquotation :: Parser Term
parseAntiquotation =
  TAntiText <$> (lexeme . try) (symbol "$str:" *> tmVar)
    <|> TAntiInt <$> (lexeme . try) (symbol "$int:" *> tmVar)

-- | Parse a Swarm language term.
parseTerm :: Parser Syntax
parseTerm = sepEndBy1 parseStmt (symbol ";") >>= mkBindChain

mkBindChain :: [Stmt] -> Parser Syntax
mkBindChain stmts = case last stmts of
  Binder x _ -> return $ foldr mkBind (STerm (TApp (TConst Pure) (TVar (lvVar x)))) stmts
  BareTerm t -> return $ foldr mkBind t (init stmts)
 where
  mkBind (BareTerm t1) t2 = loc Nothing t1 t2 $ SBind Nothing Nothing Nothing Nothing t1 t2
  mkBind (Binder x t1) t2 = loc (Just x) t1 t2 $ SBind (Just x) Nothing Nothing Nothing t1 t2
  loc mx a b = Syntax $ maybe NoLoc lvSrcLoc mx <> (a ^. sLoc) <> (b ^. sLoc)

data Stmt
  = BareTerm Syntax
  | Binder LocVar Syntax
  deriving (Show)

parseStmt :: Parser Stmt
parseStmt =
  mkStmt <$> optional (try (locTmVar <* symbol "<-")) <*> parseExpr

mkStmt :: Maybe LocVar -> Syntax -> Stmt
mkStmt Nothing = BareTerm
mkStmt (Just x) = Binder x

parseExpr :: Parser Syntax
parseExpr =
  parseLoc $ ascribe <$> parseExpr' <*> optional (symbol ":" *> parsePolytype)
 where
  ascribe :: Syntax -> Maybe RawPolytype -> Term
  ascribe s Nothing = s ^. sTerm
  ascribe s (Just ty) = SAnnotate s ty

parseExpr' :: Parser Syntax
parseExpr' = makeExprParser parseTermAtom table
 where
  table = snd <$> M.toDescList tableMap
  tableMap =
    M.unionsWith
      (++)
      [ M.singleton 9 [InfixL (exprLoc2 $ SApp <$ string "")]
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
-- >>> M.map length binOps
-- fromList [(0,1),(2,1),(3,1),(4,6),(6,3),(7,2),(8,1)]
binOps :: Map Int [Operator Parser Syntax]
binOps = M.unionsWith (++) $ mapMaybe binOpToTuple allConst
 where
  binOpToTuple c = do
    let ci = constInfo c
    ConstMBinOp assoc <- pure (constMeta ci)
    let assI = case assoc of
          L -> InfixL
          N -> InfixN
          R -> InfixR
    pure $
      M.singleton
        (fixity ci)
        [assI (mkOp c <$ operator (syntax ci))]

-- | Precedences and parsers of unary operators (currently only 'Neg').
--
-- >>> M.map length unOps
-- fromList [(7,1)]
unOps :: Map Int [Operator Parser Syntax]
unOps = M.unionsWith (++) $ mapMaybe unOpToTuple allConst
 where
  unOpToTuple c = do
    let ci = constInfo c
    ConstMUnOp assoc <- pure (constMeta ci)
    let assI = case assoc of
          P -> Prefix
          S -> Postfix
    pure $
      M.singleton
        (fixity ci)
        [assI (exprLoc1 $ SApp (noLoc $ TConst c) <$ operator (syntax ci))]

  -- combine location for ExprParser
  exprLoc1 :: Parser (Syntax -> Term) -> Parser (Syntax -> Syntax)
  exprLoc1 p = do
    (l, f) <- parseLocG p
    pure $ \s -> Syntax (l <> s ^. sLoc) $ f s
