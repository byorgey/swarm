{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Language.LSP.Hover (
  showHoverInfo,

  -- * Documentation rendering
  renderDoc,
  treeToMarkdown,

  -- * Finding source location
  narrowToPosition,

  -- * Explaining source position
  explain,
) where

import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Control.Monad (guard, void)
import Data.Foldable (asum)
import Data.Graph
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lines qualified as R
import Data.Text.Utf16.Rope.Mixed qualified as R
import Language.LSP.Protocol.Types qualified as J
import Language.LSP.VFS
import Swarm.Language.Context as Ctx
import Swarm.Language.Parser (readTerm')
import Swarm.Language.Parser.Core (defaultParserConfig)
import Swarm.Language.Pipeline (processParsedTerm)
import Swarm.Language.Syntax
import Swarm.Language.Typecheck (inferConst)
import Swarm.Language.Types
import Swarm.Pretty (prettyText, prettyTextLine)
import Swarm.Util qualified as U

withinBound :: Int -> SrcLoc -> Bool
withinBound pos (SrcLoc s e) = pos >= s && pos < e
withinBound _ NoLoc = False

ropeToLspPosition :: R.Position -> J.Position
ropeToLspPosition (R.Position l c) =
  J.Position (fromIntegral l) (fromIntegral c)

lspToRopePosition :: J.Position -> R.Position
lspToRopePosition (J.Position myLine myCol) =
  R.Position (fromIntegral myLine) (fromIntegral myCol)

showHoverInfo ::
  J.NormalizedUri ->
  J.Position ->
  VirtualFile ->
  Maybe (Text, Maybe J.Range)
showHoverInfo _ p vf@(VirtualFile _ _ myRope) =
  either (const Nothing) (fmap genHoverInfo) (readTerm' defaultParserConfig content)
 where
  content = virtualFileText vf
  absolutePos =
    R.charLength . fst $ R.charSplitAtPosition (lspToRopePosition p) myRope

  genHoverInfo stx =
    case processParsedTerm stx of
      Left _e ->
        let found = narrowToPosition stx $ fromIntegral absolutePos
            finalPos = posToRange myRope (found ^. sLoc)
         in (,finalPos) . treeToMarkdown 0 $ explain found
      Right pt ->
        let found =
              narrowToPosition pt $ fromIntegral absolutePos
            finalPos = posToRange myRope (found ^. sLoc)
         in (,finalPos) . treeToMarkdown 0 $ explain found

posToRange :: R.Rope -> SrcLoc -> Maybe J.Range
posToRange myRope foundSloc = do
  (s, e) <- case foundSloc of
    SrcLoc s e -> Just (s, e)
    _ -> Nothing
  let (startRope, _) = R.charSplitAt (fromIntegral s) myRope
      (endRope, _) = R.charSplitAt (fromIntegral e) myRope
  return $
    J.Range
      (ropeToLspPosition $ R.charLengthAsPosition startRope)
      (ropeToLspPosition $ R.charLengthAsPosition endRope)

descend ::
  ExplainableType ty =>
  -- | position
  Int ->
  -- | next element to inspect
  Syntax' ty ->
  Maybe (Syntax' ty)
descend pos s1@(Syntax' l1 _ _ _) = do
  guard $ withinBound pos l1
  return $ narrowToPosition s1 pos

-- | Find the most specific term for a given
-- position within the code.
narrowToPosition ::
  ExplainableType ty =>
  -- | parent term
  Syntax' ty ->
  -- | absolute offset within the file
  Int ->
  Syntax' ty
narrowToPosition s0@(Syntax' _ t _ ty) pos = fromMaybe s0 $ case t of
  SLam lv _ s -> d (locVarToSyntax' lv $ getInnerType ty) <|> d s
  SApp s1 s2 -> d s1 <|> d s2
  SLet _ _ lv _ _ _ s1@(Syntax' _ _ _ lty) s2 -> d (locVarToSyntax' lv lty) <|> d s1 <|> d s2
  SBind mlv _ _ _ s1@(Syntax' _ _ _ lty) s2 -> (mlv >>= d . flip locVarToSyntax' (getInnerType lty)) <|> d s1 <|> d s2
  STydef typ typBody _ti s1 -> d s1 <|> Just (locVarToSyntax' typ $ fromPoly typBody)
  SPair s1 s2 -> d s1 <|> d s2
  SDelay s -> d s
  SRcd m -> asum . map d . catMaybes . M.elems $ m
  SProj s1 _ -> d s1
  SAnnotate s _ -> d s
  SRequirements _ s -> d s
  -- atoms - return their position and end recursion
  TUnit -> Nothing
  TConst {} -> Nothing
  TDir {} -> Nothing
  TInt {} -> Nothing
  TText {} -> Nothing
  TBool {} -> Nothing
  TVar {} -> Nothing
  TRequire {} -> Nothing
  TRequireDevice {} -> Nothing
  TType {} -> Nothing
  SImportIn {} -> Nothing
  -- these should not show up in surface language
  TRef {} -> Nothing
  TRobot {} -> Nothing
  TAntiInt {} -> Nothing
  TAntiText {} -> Nothing
  SSuspend {} -> Nothing
 where
  d = descend pos

renderDoc :: Int -> Text -> Text
renderDoc d t
  | d == 0 = t
  | otherwise = T.drop 2 . indent (max 0 (4 * (d - 1)) + 2) $ "* " <> t
 where
  indent x = T.unlines . map (T.replicate x " " <>) . T.lines

treeToMarkdown :: Int -> Tree Text -> Text
treeToMarkdown d (Node t children) =
  T.unlines $ renderDoc d t : map (treeToMarkdown $ d + 1) children

class Show t => ExplainableType t where
  fromPoly :: Polytype -> t

  -- | Pretty print the type.
  prettyType :: t -> Text

  -- | Strip the type of its outermost layer.
  --
  -- This allows us to strip lambda or command type
  -- and get the type of the bound variable.
  getInnerType :: t -> t

  -- | Check if this type is same as the given 'Polytype'.
  --
  -- We use it to not print same type twice (e.g. inferred and generic).
  eq :: t -> Polytype -> Bool

instance ExplainableType () where
  fromPoly = const ()
  prettyType = const "?"
  getInnerType = id
  eq _ _ = False

instance ExplainableType Polytype where
  fromPoly = id
  prettyType = prettyTextLine
  getInnerType = fmap $ \case
    (l :->: _r) -> l
    (TyCmd t) -> t
    t -> t
  eq = (==)

instance ExplainableType RawPolytype where
  fromPoly = forgetQ
  prettyType = prettyTextLine
  getInnerType = fmap $ \case
    (l :->: _r) -> l
    (TyCmd t) -> t
    t -> t
  eq r t = r == forgetQ t

explain :: ExplainableType ty => Syntax' ty -> Tree Text
explain trm = case trm ^. sTerm of
  TUnit -> literal "The unit value."
  TConst c -> literal . constGenSig c $ briefDoc (constDoc $ constInfo c)
  TDir {} -> literal "A direction literal."
  TInt {} -> literal "An integer literal."
  TText {} -> literal "A text literal."
  TBool {} -> literal "A boolean literal."
  TVar v -> pure $ typeSignature v ty ""
  SRcd {} -> literal "A record literal."
  SProj {} -> literal "A record projection."
  STydef {} -> literal "A type synonym definition."
  TType {} -> literal "A type literal."
  SImportIn {} -> literal "An import expression."
  -- type ascription
  SAnnotate lhs typeAnn ->
    Node
      (typeSignature "_" typeAnn "A type ascription for")
      [explain lhs]
  -- special forms (function application will show for `$`, but really should be rare)
  SApp {} -> explainFunction trm
  TRequireDevice {} -> pure "Require a specific device to be equipped."
  TRequire {} -> pure "Require a certain number of an entity."
  SRequirements {} -> pure "Query the requirements of a term."
  -- definition or bindings
  SLet ls isRecursive var mTypeAnn _ _ rhs _b -> pure $ explainDefinition ls isRecursive var (rhs ^. sType) mTypeAnn
  SLam (LV _s v) _mType _syn ->
    pure $
      typeSignature v ty $
        "A lambda expression binding the variable " <> U.bquote v <> "."
  SBind mv _ _ _ rhs _cmds ->
    pure $
      typeSignature (maybe "__rhs" lvVar mv) (getInnerType $ rhs ^. sType) $
        "A monadic bind for commands" <> maybe "." (\(LV _s v) -> ", that binds variable " <> U.bquote v <> ".") mv
  -- composite types
  SPair {} ->
    Node
      (typeSignature "_" ty "A tuple consisting of:")
      (explain <$> unTuple trm)
  SDelay {} ->
    pure . T.unlines $
      [ "Delay evaluation of a term, written `{...}`."
      , ""
      , "Swarm is an eager language, but in some cases (e.g. for `if` statements and recursive bindings) we need to delay evaluation."
      , ""
      , "The counterpart to `{...}` is `force`:"
      , "```"
      , "force {t} = t"
      , "```"
      ]
  -- internal syntax that should not actually show in hover
  TRef {} -> internal "A memory reference."
  TAntiInt {} -> internal "An antiquoted Haskell variable name of type Integer."
  TAntiText {} -> internal "An antiquoted Haskell variable name of type Text."
  TRobot {} -> internal "A robot reference."
  SSuspend {} -> internal "A suspension."
 where
  ty = trm ^. sType
  literal = pure . typeSignature (prettyText . void $ trm ^. sTerm) ty
  internal description = literal $ description <> "\n**These should never show up in surface syntax.**"
  constGenSig c =
    let ity = inferConst c
     in U.applyWhen (not $ ty `eq` ity) $ typeSignature (prettyText c) ity

-- | Helper function to explain function application.
--
-- Note that 'Force' is often inserted internally, so
-- if it shows up here we drop it.
explainFunction :: ExplainableType ty => Syntax' ty -> Tree Text
explainFunction s =
  case unfoldApps s of
    (Syntax' _ (TConst Force) _ _ :| [innerT]) -> explain innerT
    (Syntax' _ (TConst Force) _ _ :| f : params) -> explainF f params
    (f :| params) -> explainF f params
 where
  explainF f params =
    Node
      "Function application of:"
      [ explain f
      , Node
          "with parameters:"
          (map explain params)
      ]

explainDefinition :: ExplainableType ty => LetSyntax -> Bool -> LocVar -> ty -> Maybe RawPolytype -> Text
explainDefinition ls isRecursive (LV _s var) ty maybeTypeAnnotation =
  typeSignature var ty $
    T.unwords
      [ "A"
      , (if isRecursive then "" else "non-") <> "recursive"
      , if ls == LSDef then "definition" else "let"
      , "expression"
      , if isNothing maybeTypeAnnotation then "without" else "with"
      , "a type annotation on the variable."
      ]

typeSignature :: ExplainableType ty => Var -> ty -> Text -> Text
typeSignature v typ body = T.unlines ["```", short, "```", body]
 where
  short = v <> ": " <> prettyType typ
