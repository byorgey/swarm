{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm requirements analysis tests
module TestRequirements where

import Control.Lens (view)
import Data.Set qualified as S
import Data.Text (Text)
import Swarm.Language.Capability
import Swarm.Language.Context qualified as Ctx
import Swarm.Language.Pipeline
import Swarm.Language.Requirements.Type (ReqCtx, Requirements)
import Test.Tasty
import Test.Tasty.HUnit
import TestUtil (check)

testRequirements :: TestTree
testRequirements =
  testGroup
    "Requirements analysis"
    [ testGroup
        "Basic capabilities"
        [ testCase "solar panel" $ "noop" `requiresCap` CPower
        , testCase "move" $ "move" `requiresCap` CMove
        , testCase "lambda" $ "\\x. x" `requiresCap` CLambda
        , testCase "inl" $ "inl 3" `requiresCap` CSum
        , testCase "cap from type" $ "inl () : rec t. Unit + t" `requiresCap` CRectype
        ]
    , testGroup
        "Scope"
        [ testCase "global var requirement does not apply to local var (#1914)" $
            checkReqCtx
              "def m = move end; def y = \\m. log (format m) end"
              (maybe False ((CMove `S.notMember`) . capReqs) . Ctx.lookup "y")
        ]
    ]

checkReqCtx :: Text -> (ReqCtx -> Bool) -> Assertion
checkReqCtx code expect = check code (expect . view processedReqCtx)

checkRequirements :: Text -> (Requirements -> Bool) -> Assertion
checkRequirements code expect = check code (expect . view processedRequirements)

requiresCap :: Text -> Capability -> Assertion
requiresCap code cap = checkRequirements code ((cap `S.member`) . capReqs)
