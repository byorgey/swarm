{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Some convenient functions for putting together the whole Swarm
-- language processing pipeline: parsing, type checking, capability
-- checking, and elaboration.  If you want to simply turn some raw
-- text representing a Swarm program into something useful, this is
-- probably the module you want.
module Swarm.Language.Pipeline (
  -- * Contexts
  Contexts (..),

  -- * ProcessedTerm
  ProcessedTerm (..),
  processedModule,
  processedSyntax,
  processedRequirements,
  processedReqCtx,

  -- * Pipeline functions for producing ProcessedTerm
  processTerm,
  processParsedTerm,
  processTerm',
  processParsedTerm',
  processTermEither,
) where

import Control.Lens (Lens', makeLenses, view, (^.))
import Data.Bifunctor (first)
import Data.Data (Data)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml as Y
import GHC.Generics (Generic)
import Swarm.Language.Elaborate
import Swarm.Language.Module
import Swarm.Language.Parser (readTerm)
import Swarm.Language.Pretty
import Swarm.Language.Requirement
import Swarm.Language.Syntax
import Swarm.Language.Typecheck
import Swarm.Language.Types
import Witch (into)

data Contexts = Contexts
  { _tCtx :: TCtx
  , _reqCtx :: ReqCtx
  , _tydefCtx :: TDCtx
  }
  deriving (Eq, Show, Generic, Data, ToJSON, FromJSON)

makeLenses ''Contexts

instance Semigroup Contexts where
  Contexts t1 r1 y1 <> Contexts t2 r2 y2 = Contexts (t1 <> t2) (r1 <> r2) (y1 <> y2)

instance Monoid Contexts where
  mempty = Contexts mempty mempty mempty

-- | A record containing the results of the language processing
--   pipeline.  Put a 'Term' in, and get one of these out.  A
--   'ProcessedTerm' contains:
--
--   * The elaborated + type-annotated term, plus the types of any
--     embedded definitions ('TModule')
--
--   * The 'Requirements' of the term
--
--   * The requirements context for any definitions embedded in the
--     term ('ReqCtx')
data ProcessedTerm = ProcessedTerm
  { _processedModule :: TModule
  , _processedRequirements :: Requirements
  , _processedReqCtx :: ReqCtx
  }
  deriving (Data, Show, Eq, Generic)

makeLenses ''ProcessedTerm

-- | A convenient lens directly targeting the AST stored in a
--   ProcessedTerm.
processedSyntax :: Lens' ProcessedTerm (Syntax' Polytype)
processedSyntax = processedModule . moduleSyntax

processTermEither :: Text -> Either Text ProcessedTerm
processTermEither t = case processTerm t of
  Left err -> Left $ T.unwords ["Could not parse term:", err]
  Right Nothing -> Left "Term was only whitespace"
  Right (Just pt) -> Right pt

instance FromJSON ProcessedTerm where
  parseJSON = withText "Term" $ either (fail . into @String) return . processTermEither

instance ToJSON ProcessedTerm where
  toJSON = String . prettyText . view processedSyntax

-- | Given a 'Text' value representing a Swarm program,
--
--   1. Parse it (see "Swarm.Language.Parse")
--   2. Typecheck it (see "Swarm.Language.Typecheck")
--   3. Elaborate it (see "Swarm.Language.Elaborate")
--   4. Check what capabilities it requires (see "Swarm.Language.Capability")
--
--   Return either the end result (or @Nothing@ if the input was only
--   whitespace) or a pretty-printed error message.
processTerm :: Text -> Either Text (Maybe ProcessedTerm)
processTerm = processTerm' mempty

-- | Like 'processTerm', but use a term that has already been parsed.
processParsedTerm :: Syntax -> Either ContextualTypeErr ProcessedTerm
processParsedTerm = processParsedTerm' mempty

-- | Like 'processTerm', but use explicit starting contexts.
processTerm' :: Contexts -> Text -> Either Text (Maybe ProcessedTerm)
processTerm' ctxs txt = do
  mt <- readTerm txt
  first (prettyTypeErrText txt) $ traverse (processParsedTerm' ctxs) mt

-- | Like 'processTerm'', but use a term that has already been parsed.
processParsedTerm' :: Contexts -> Syntax -> Either ContextualTypeErr ProcessedTerm
processParsedTerm' ctxs t = do
  m <- inferTop (ctxs ^. tCtx) (ctxs ^. tydefCtx) t
  let (caps, reqCtx') = requirements (ctxs ^. reqCtx) (t ^. sTerm)
  return $ ProcessedTerm (elaborateModule m) caps reqCtx'

elaborateModule :: TModule -> TModule
elaborateModule (Module ast ctx tydefs) = Module (elaborate ast) ctx tydefs
