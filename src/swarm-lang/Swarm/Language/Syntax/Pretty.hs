{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- We could avoid orphan instances by placing these PrettyPrec
-- instances in Swarm.Language.Syntax.AST, along with the declarations
-- of the types we are making PrettyPrec instances for, but the code
-- here depends nontrivially on stuff in other Swarm.Language.Syntax.*
-- modules, so that would require putting a whole bunch of stuff all
-- in the same module.  It seemed like a much better option to have
-- code split out into separate modules and disable orphan instance
-- warnings in this one.

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Pretty-printing for terms in the Swarm programming language.
module Swarm.Language.Syntax.Pretty (

) where

import Control.Lens.Empty (pattern Empty)
import Data.Bool (bool)
import Data.Foldable qualified as F
import Data.Map qualified as M
import Data.Sequence qualified as Seq
import Data.String (fromString)
import Prettyprinter
import Swarm.Language.Syntax.AST
import Swarm.Language.Syntax.Comments
import Swarm.Language.Syntax.Constants
import Swarm.Language.Syntax.Loc
import Swarm.Language.Syntax.Pattern (pattern STerm)
import Swarm.Language.Syntax.Util (erase, unTuple)
import Swarm.Language.Types
import Swarm.Pretty (PrettyPrec (..), encloseWithIndent, pparens, ppr, prettyEquality)
import Text.Show.Unicode (ushow)

-- | Pretty-print a syntax node with comments.
instance PrettyPrec (Syntax' ty) where
  prettyPrec p (Syntax' _ t (Comments before after) _) = case before of
    Empty -> t'
    _ ->
      -- Print out any comments before the node, with a blank line before
      mconcat
        [ hardline
        , vsep (map ppr (F.toList before))
        , hardline
        , t'
        ]
   where
    -- Print the node itself, possibly with suffix comments on the same line
    t' = case Seq.viewr after of
      Seq.EmptyR -> prettyPrec p t
      _ Seq.:> lst -> case commentType lst of
        -- Output a newline after a line comment, but not after a block comment
        BlockComment -> tWithComments
        LineComment -> tWithComments <> hardline
     where
      -- The pretty-printed node with suffix comments
      tWithComments = prettyPrec p t <+> hsep (map ppr (F.toList after))

instance PrettyPrec (Term' ty) where
  prettyPrec p = \case
    TUnit -> "()"
    TConst c -> prettyPrec p c
    TDir d -> ppr d
    TInt n -> pretty n
    TAntiInt v -> "$int:" <> pretty v
    TText s -> fromString (ushow s)
    TAntiText v -> "$str:" <> pretty v
    TBool b -> bool "false" "true" b
    TRobot r -> "<a" <> pretty r <> ">"
    TRef r -> "@" <> pretty r
    TRequireDevice d -> pparens (p > 10) $ "require" <+> ppr (TText d)
    TRequire n e -> pparens (p > 10) $ "require" <+> pretty n <+> ppr (TText e)
    SRequirements _ e -> pparens (p > 10) $ "requirements" <+> ppr e
    TVar s -> pretty s
    SDelay (Syntax' _ (TConst Noop) _ _) -> "{}"
    SDelay t -> group . encloseWithIndent 2 lbrace rbrace $ ppr t
    t@SPair {} -> prettyTuple t
    t@SLam {} ->
      pparens (p > 9) $
        prettyLambdas t
    -- Special handling of infix operators - ((+) 2) 3 --> 2 + 3
    SApp t@(Syntax' _ (SApp (Syntax' _ (TConst c) _ _) l) _ _) r ->
      let ci = constInfo c
          pC = fixity ci
       in case constMeta ci of
            ConstMBinOp assoc ->
              pparens (p > pC) $
                hsep
                  [ prettyPrec (pC + fromEnum (assoc == R)) l
                  , ppr c
                  , prettyPrec (pC + fromEnum (assoc == L)) r
                  ]
            _ -> prettyPrecApp p t r
    SApp t1 t2 -> case t1 of
      Syntax' _ (TConst c) _ _ ->
        let ci = constInfo c
            pC = fixity ci
         in case constMeta ci of
              ConstMUnOp P -> pparens (p > pC) $ ppr t1 <> prettyPrec (succ pC) t2
              ConstMUnOp S -> pparens (p > pC) $ prettyPrec (succ pC) t2 <> ppr t1
              _ -> prettyPrecApp p t1 t2
      _ -> prettyPrecApp p t1 t2
    SLet LSLet _ (LV _ x) mty _ _ t1 t2 ->
      sep
        [ prettyDefinition "let" x mty t1 <+> "in"
        , ppr t2
        ]
    SLet LSDef _ (LV _ x) mty _ _ t1 t2 ->
      mconcat $
        sep [prettyDefinition "def" x mty t1, "end"]
          : case t2 of
            Syntax' _ (TConst Noop) _ _ -> []
            _ -> [hardline, hardline, ppr t2]
    STydef (LV _ x) pty _ t1 ->
      mconcat $
        prettyTydef x pty
          : case t1 of
            Syntax' _ (TConst Noop) _ _ -> []
            _ -> [hardline, hardline, ppr t1]
    SBind Nothing _ _ _ t1 t2 ->
      pparens (p > 0) $
        prettyPrec 1 t1 <> ";" <> line <> prettyPrec 0 t2
    SBind (Just (LV _ x)) _ _ _ t1 t2 ->
      pparens (p > 0) $
        pretty x <+> "<-" <+> prettyPrec 1 t1 <> ";" <> line <> prettyPrec 0 t2
    SRcd m -> brackets $ hsep (punctuate "," (map prettyEquality (M.assocs m)))
    SProj t x -> prettyPrec 11 t <> "." <> pretty x
    SAnnotate t pt ->
      pparens (p > 0) $
        prettyPrec 1 t <+> ":" <+> ppr pt
    SSuspend t ->
      pparens (p > 10) $
        "suspend" <+> prettyPrec 11 t
    TType ty -> prettyPrec p ty
    SImportIn _x _t -> error "XXX unimplemented: pretty SImportIn"

prettyDefinition :: Doc ann -> Var -> Maybe (Poly q Type) -> Syntax' ty -> Doc ann
prettyDefinition defName x mty t1 =
  nest 2 . sep $
    [ flatAlt
        (defHead <> group defType <+> eqAndLambdaLine)
        (defHead <> group defType' <+> defEqLambdas)
    , ppr defBody
    ]
 where
  (defBody, defLambdaList) = unchainLambdas t1
  defHead = defName <+> pretty x
  defType = maybe "" (\ty -> ":" <+> flatAlt (line <> indent 2 (ppr ty)) (ppr ty)) mty
  defType' = maybe "" (\ty -> ":" <+> ppr ty) mty
  defEqLambdas = hsep ("=" : map prettyLambda defLambdaList)
  eqAndLambdaLine = if null defLambdaList then "=" else line <> defEqLambdas

prettyTydef :: Var -> Polytype -> Doc ann
prettyTydef x (unPoly -> ([], ty)) = "tydef" <+> pretty x <+> "=" <+> ppr ty <+> "end"
prettyTydef x (unPoly -> (xs, ty)) = "tydef" <+> pretty x <+> hsep (map pretty xs) <+> "=" <+> ppr ty <+> "end"

prettyPrecApp :: Int -> Syntax' ty -> Syntax' ty -> Doc a
prettyPrecApp p t1 t2 =
  pparens (p > 10) $
    prettyPrec 10 t1 <+> prettyPrec 11 t2

prettyTuple :: Term' ty -> Doc a
prettyTuple = tupled . map ppr . unTuple . STerm . erase

prettyLambdas :: Term' ty -> Doc a
prettyLambdas t = hsep (prettyLambda <$> lms) <> softline <> ppr rest
 where
  (rest, lms) = unchainLambdas (STerm (erase t))

unchainLambdas :: Syntax' ty -> (Syntax' ty, [(Var, Maybe Type)])
unchainLambdas = \case
  Syntax' _ (SLam (LV _ x) mty body) _ _ -> ((x, mty) :) <$> unchainLambdas body
  body -> (body, [])

prettyLambda :: (Pretty a1, PrettyPrec a2) => (a1, Maybe a2) -> Doc ann
prettyLambda (x, mty) = "\\" <> pretty x <> maybe "" ((":" <>) . ppr) mty <> "."
