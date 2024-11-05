{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm unit tests for contexts
module TestContext where

import Data.Map qualified as M
import Swarm.Language.Context
import Swarm.Util (showT)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)

testContext :: TestTree
testContext =
  testGroup
    "Contexts"
    [ testGroup
        "Context equality"
        [ testCase "idempotence 1" $ ctxsEqual ctx1 (ctx1 <> ctx1)
        , testCase "idempotence 2" $ ctxsEqual ctx2 (ctx2 <> ctx2)
        , testCase "deletion" $ ctxsEqual ctx1 (delete "z" ctx2)
        , testCase "empty/delete" $ ctxsEqual empty (delete "x" ctx1)
        , testCase "fromMap" $ ctxsEqual ctx2 (fromMap (M.fromList [("x", 3), ("z", 6)]))
        , testCase "right bias" $ ctxsEqual ctx4 (ctx2 <> ctx3)
        , testCase "commutativity" $ ctxsEqual (ctx1 <> ctx5) (ctx5 <> ctx1)
        ]
    , testGroup
        "de/rehydrate round-trip"
        [ testCase "empty" $ serializeRoundTrip empty
        , testCase "ctx1" $ serializeRoundTrip ctx1
        , testCase "ctx2" $ serializeRoundTrip ctx2
        , testCase "ctx3" $ serializeRoundTrip ctx3
        , testCase "ctx4" $ serializeRoundTrip ctx4
        , testCase "ctx5" $ serializeRoundTrip ctx5
        , testCase "large" $ serializeRoundTrip bigCtx
        , testCase "delete" $ serializeRoundTrip (delete "y" ctx4)
        ]
    ]
 where
  ctx1 = singleton "x" 3
  ctx2 = singleton "x" 3 <> singleton "z" 6
  ctx3 = singleton "x" 5 <> singleton "y" 7
  ctx4 = singleton "x" 5 <> singleton "y" 7 <> singleton "z" 6
  ctx5 = singleton "y" 10
  bigCtx = fromMap . M.fromList $ zip (map (("x" <>) . showT) [1 :: Int ..]) [1 .. 10000]

ctxsEqual :: Ctx Int -> Ctx Int -> Assertion
ctxsEqual ctx1 ctx2 = do
  -- Contexts are compared by hash for equality
  assertEqual "hash equality" ctx1 ctx2

  -- Make sure they are also structurally equal
  assertBool "structural equality" (ctxStructEqual ctx1 ctx2)
 where
  ctxStructEqual (Ctx m1 _) (Ctx m2 _) = m1 == m2

serializeRoundTrip :: Ctx Int -> Assertion
serializeRoundTrip ctx = do
  case getCtx (ctxHash ctx) (rehydrate (dehydrate (toCtxMap ctx))) of
    Nothing -> fail "Failed to reconstitute dehydrated context"
    Just ctx' -> ctxsEqual ctx ctx'
