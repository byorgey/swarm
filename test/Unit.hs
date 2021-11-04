{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Swarm unit tests
module Main where

import Control.Lens ((&), (.~))
import Control.Monad.Except
import Control.Monad.State
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Linear
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Witch (from)

import Swarm.Game.CESK
import Swarm.Game.Exception
import Swarm.Game.Robot
import Swarm.Game.State
import Swarm.Game.Step
import Swarm.Game.Value
import Swarm.Language.Context
import Swarm.Language.Pipeline (ProcessedTerm (..), processTerm)
import Swarm.Language.Pretty
import Swarm.Language.Syntax hiding (mkOp)
import Swarm.TUI.Model

main :: IO ()
main = do
  mg <- runExceptT (initGameState 0)
  case mg of
    Left err -> assertFailure (from err)
    Right g -> defaultMain (tests g)

tests :: GameState -> TestTree
tests g = testGroup "Tests" [parser, prettyConst, eval g, testModel]

parser :: TestTree
parser =
  testGroup
    "Language - pipeline"
    [ testCase "end semicolon #79" (valid "def a = 41 end def b = a + 1 end def c = b + 2 end")
    , testCase
        "quantification #148 - implicit"
        (valid "def id : a -> a = \\x. x end; id move")
    , testCase
        "quantification #148 - explicit"
        (valid "def id : forall a. a -> a = \\x. x end; id move")
    , testCase
        "quantification #148 - explicit with free tyvars"
        ( process
            "def id : forall a. b -> b = \\x. x end; id move"
            ( T.unlines
                [ "1:27:"
                , "  |"
                , "1 | def id : forall a. b -> b = \\x. x end; id move"
                , "  |                           ^"
                , "  Type contains free variable(s): b"
                , "  Try adding them to the 'forall'."
                , ""
                ]
            )
        )
    , testCase
        "parsing operators #188 - parse valid operator (!=)"
        (valid "1!=(2)")
    , testCase
        "parsing operators #236 - parse valid operator (<=)"
        (valid "1 <= 2")
    , testCase
        "parsing operators #239 - parse valid operator ($)"
        (valid "fst $ snd $ (1,2,3)")
    , testCase
        "Allow ' in variable names #269 - parse variable name containing '"
        (valid "def a'_' = 0 end")
    , testCase
        "Allow ' in variable names #269 - do not parse variable starting with '"
        ( process
            "def 'a = 0 end"
            ( T.unlines
                [ "1:5:"
                , "  |"
                , "1 | def 'a = 0 end"
                , "  |     ^"
                , "unexpected '''"
                , "expecting variable name"
                ]
            )
        )
    , testGroup
        "failure location - #268"
        [ testCase
            "located type error"
            ( process
                "def a =\n 42 + \"oops\"\nend"
                "2: Can't unify int and string"
            )
        , testCase
            "failure inside bind chain"
            ( process
                "move;\n1;\nmove"
                "2: Can't unify int and cmd"
            )
        , testCase
            "failure inside function call"
            ( process
                "if true \n{} \n(move)"
                "3: Can't unify {u0} and cmd ()"
            )
        , testCase
            "parsing operators #236 - report failure on invalid operator start"
            ( process
                "1 <== 2"
                ( T.unlines
                    [ "1:3:"
                    , "  |"
                    , "1 | 1 <== 2"
                    , "  |   ^"
                    , "unexpected '<'"
                    ]
                )
            )
        ]
    ]
 where
  valid = flip process ""

  process :: Text -> Text -> Assertion
  process code expect = case processTerm code of
    Left e
      | not (T.null expect) && expect `T.isPrefixOf` e -> pure ()
      | otherwise -> error $ "Unexpected failure: " <> show e
    Right _
      | expect == "" -> pure ()
      | otherwise -> error "Unexpected success"

prettyConst :: TestTree
prettyConst =
  testGroup
    "Language - pretty"
    [ testCase
        "operators #8 - function application unchanged"
        ( equalPretty "f say" $
            TApp (TVar "f") (TConst Say)
        )
    , testCase
        "operators #8 - double function application unchanged"
        ( equalPretty "f () ()" $
            TApp (TApp (TVar "f") TUnit) TUnit
        )
    , testCase
        "operators #8 - embrace operator parameter"
        ( equalPretty "f (==)" $
            TApp (TVar "f") (TConst Eq)
        )
    , testCase
        "operators #8 - unary negation"
        ( equalPretty "-3" $
            TApp (TConst Neg) (TInt 3)
        )
    , testCase
        "operators #8 - double unary negation"
        ( equalPretty "-(-1)" $
            TApp (TConst Neg) $ TApp (TConst Neg) (TInt 1)
        )
    , testCase
        "operators #8 - unary negation with strongly fixing binary operator"
        ( equalPretty "-1 ^ (-2)" $
            TApp (TConst Neg) $ mkOp' Exp (TInt 1) $ TApp (TConst Neg) (TInt 2)
        )
    , testCase
        "operators #8 - unary negation with weakly fixing binary operator"
        ( equalPretty "-(1 + -2)" $
            TApp (TConst Neg) $ mkOp' Add (TInt 1) $ TApp (TConst Neg) (TInt 2)
        )
    , testCase
        "operators #8 - simple infix operator"
        ( equalPretty "1 == 2" $
            mkOp' Eq (TInt 1) (TInt 2)
        )
    , testCase
        "operators #8 - infix operator with less fixing inner operator"
        ( equalPretty "1 * (2 + 3)" $
            mkOp' Mul (TInt 1) (mkOp' Add (TInt 2) (TInt 3))
        )
    , testCase
        "operators #8 - infix operator with more fixing inner operator"
        ( equalPretty "1 + 2 * 3" $
            mkOp' Add (TInt 1) (mkOp' Mul (TInt 2) (TInt 3))
        )
    , testCase
        "operators #8 - infix operator right associativity"
        ( equalPretty "2 ^ 4 ^ 8" $
            mkOp' Exp (TInt 2) (mkOp' Exp (TInt 4) (TInt 8))
        )
    , testCase
        "operators #8 - infix operator right associativity not applied to left"
        ( equalPretty "(2 ^ 4) ^ 8" $
            mkOp' Exp (mkOp' Exp (TInt 2) (TInt 4)) (TInt 8)
        )
    ]
 where
  equalPretty :: String -> Term -> Assertion
  equalPretty expected term = assertEqual "" expected . show $ ppr term

binOp :: Show a => a -> String -> a -> Text
binOp a op b = from @String (p (show a) ++ op ++ p (show b))
 where
  p x = "(" ++ x ++ ")"

eval :: GameState -> TestTree
eval g =
  testGroup
    "Language - evaluation"
    [ testGroup
        "arithmetic"
        [ testProperty
            "addition"
            (\a b -> binOp a "+" b `evaluatesToP` VInt (a + b))
        , testProperty
            "subtraction"
            (\a b -> binOp a "-" b `evaluatesToP` VInt (a - b))
        , testProperty
            "multiplication"
            (\a b -> binOp a "*" b `evaluatesToP` VInt (a * b))
        , testProperty
            "division"
            (\a (NonZero b) -> binOp a "/" b `evaluatesToP` VInt (a `div` b))
        , testProperty
            "exponentiation"
            (\a (NonNegative b) -> binOp a "^" b `evaluatesToP` VInt (a ^ (b :: Integer)))
        ]
    , testGroup
        "int comparison"
        [ testProperty
            "=="
            (\a b -> binOp a "==" b `evaluatesToP` VBool ((a :: Integer) == b))
        , testProperty
            "<"
            (\a b -> binOp a "<" b `evaluatesToP` VBool ((a :: Integer) < b))
        , testProperty
            "<="
            (\a b -> binOp a "<=" b `evaluatesToP` VBool ((a :: Integer) <= b))
        , testProperty
            ">"
            (\a b -> binOp a ">" b `evaluatesToP` VBool ((a :: Integer) > b))
        , testProperty
            ">="
            (\a b -> binOp a ">=" b `evaluatesToP` VBool ((a :: Integer) >= b))
        , testProperty
            "!="
            (\a b -> binOp a "!=" b `evaluatesToP` VBool ((a :: Integer) /= b))
        ]
    , testGroup
        "pair comparison"
        [ testProperty
            "=="
            (\a b -> binOp a "==" b `evaluatesToP` VBool ((a :: (Integer, Integer)) == b))
        , testProperty
            "<"
            (\a b -> binOp a "<" b `evaluatesToP` VBool ((a :: (Integer, Integer)) < b))
        , testProperty
            "<="
            (\a b -> binOp a "<=" b `evaluatesToP` VBool ((a :: (Integer, Integer)) <= b))
        , testProperty
            ">"
            (\a b -> binOp a ">" b `evaluatesToP` VBool ((a :: (Integer, Integer)) > b))
        , testProperty
            ">="
            (\a b -> binOp a ">=" b `evaluatesToP` VBool ((a :: (Integer, Integer)) >= b))
        , testProperty
            "!="
            (\a b -> binOp a "!=" b `evaluatesToP` VBool ((a :: (Integer, Integer)) /= b))
        ]
    , testGroup
        "sum types #224"
        [ testCase
            "inl"
            ("inl 3" `evaluatesTo` VInj False (VInt 3))
        , testCase
            "inr"
            ("inr \"hi\"" `evaluatesTo` VInj True (VString "hi"))
        , testCase
            "inl a < inl b"
            ("inl 3 < inl 4" `evaluatesTo` VBool True)
        , testCase
            "inl b < inl a"
            ("inl 4 < inl 3" `evaluatesTo` VBool False)
        , testCase
            "inl < inr"
            ("inl 3 < inr true" `evaluatesTo` VBool True)
        , testCase
            "inl 4 < inr 3"
            ("inl 4 < inr 3" `evaluatesTo` VBool True)
        , testCase
            "inr < inl"
            ("inr 3 < inl true" `evaluatesTo` VBool False)
        , testCase
            "inr 3 < inl 4"
            ("inr 3 < inl 4" `evaluatesTo` VBool False)
        , testCase
            "inr a < inr b"
            ("inr 3 < inr 4" `evaluatesTo` VBool True)
        , testCase
            "inr b < inr a"
            ("inr 4 < inr 3" `evaluatesTo` VBool False)
        , testCase
            "case inl"
            ("case (inl 2) (\\x. x + 1) (\\y. y * 17)" `evaluatesTo` VInt 3)
        , testCase
            "case inr"
            ("case (inr 2) (\\x. x + 1) (\\y. y * 17)" `evaluatesTo` VInt 34)
        , testCase
            "nested 1"
            ("(\\x : int + bool + string. case x (\\q. 1) (\\s. case s (\\y. 2) (\\z. 3))) (inl 3)" `evaluatesTo` VInt 1)
        , testCase
            "nested 2"
            ("(\\x : int + bool + string. case x (\\q. 1) (\\s. case s (\\y. 2) (\\z. 3))) (inr (inl false))" `evaluatesTo` VInt 2)
        , testCase
            "nested 2"
            ("(\\x : int + bool + string. case x (\\q. 1) (\\s. case s (\\y. 2) (\\z. 3))) (inr (inr \"hi\"))" `evaluatesTo` VInt 3)
        ]
    , testGroup
        "operator evaluation"
        [ testCase
            "application operator #239"
            ("fst $ snd $ (1,2,3)" `evaluatesTo` VInt 2)
        ]
    , testGroup
        "recursive bindings"
        [ testCase
            "factorial"
            ("let fac = \\n. if (n==0) {1} {n * fac (n-1)} in fac 15" `evaluatesTo` VInt 1307674368000)
        , testCase
            "loop detected"
            ("let x = x in x" `throwsError` ("loop detected" `T.isInfixOf`))
        ]
    , testGroup
        "delay"
        [ testCase
            "force / delay"
            ("force {10}" `evaluatesTo` VInt 10)
        , testCase
            "force x2 / delay x2"
            ("force (force { {10} })" `evaluatesTo` VInt 10)
        , testCase
            "if is lazy"
            ("if true {1} {1/0}" `evaluatesTo` VInt 1)
        , testCase
            "function with if is not lazy"
            ( "let f = \\x. \\y. if true {x} {y} in f 1 (1/0)"
                `throwsError` ("by zero" `T.isInfixOf`)
            )
        , testCase
            "memoization baseline"
            ( "def fac = \\n. if (n==0) {1} {n * fac (n-1)} end; def f10 = fac 10 end; let x = f10 in noop"
                `evaluatesToInAtMost` (VUnit, 535)
            )
        , testCase
            "memoization"
            ( "def fac = \\n. if (n==0) {1} {n * fac (n-1)} end; def f10 = fac 10 end; let x = f10 in let y = f10 in noop"
                `evaluatesToInAtMost` (VUnit, 540)
            )
        ]
    , testGroup
        "conditions"
        [ testCase
            "if true"
            ("if true {1} {2}" `evaluatesTo` VInt 1)
        , testCase
            "if false"
            ("if false {1} {2}" `evaluatesTo` VInt 2)
        , testCase
            "if (complex condition)"
            ("if (let x = 3 + 7 in not (x < 2^5)) {1} {2}" `evaluatesTo` VInt 2)
        ]
    , testGroup
        "exceptions"
        [ testCase
            "raise"
            ("raise \"foo\"" `throwsError` ("foo" `T.isInfixOf`))
        , testCase
            "try / no exception 1"
            ("try {return 1} {return 2}" `evaluatesTo` VInt 1)
        , testCase
            "try / no exception 2"
            ("try {return 1} {let x = x in x}" `evaluatesTo` VInt 1)
        , testCase
            "try / raise"
            ("try {raise \"foo\"} {return 3}" `evaluatesTo` VInt 3)
        , testCase
            "try / raise / raise"
            ("try {raise \"foo\"} {raise \"bar\"}" `throwsError` ("bar" `T.isInfixOf`))
        , testCase
            "try / div by 0"
            ("try {return (1/0)} {return 3}" `evaluatesTo` VInt 3)
        ]
    ]
 where
  throwsError :: Text -> (Text -> Bool) -> Assertion
  throwsError tm p = do
    result <- evaluate tm
    case result of
      Right _ -> assertFailure "Unexpected success"
      Left err ->
        p err
          @? "Expected predicate did not hold on error message " ++ from @Text @String err

  evaluatesTo :: Text -> Value -> Assertion
  evaluatesTo tm val = do
    result <- evaluate tm
    assertEqual "" (Right val) (fst <$> result)

  evaluatesToP :: Text -> Value -> Property
  evaluatesToP tm val = ioProperty $ do
    result <- evaluate tm
    return $ Right val == (fst <$> result)

  evaluatesToInAtMost :: Text -> (Value, Int) -> Assertion
  evaluatesToInAtMost tm (val, maxSteps) = do
    result <- evaluate tm
    case result of
      Left err -> assertFailure ("Evaluation failed: " ++ from @Text @String err)
      Right (v, steps) -> do
        assertEqual "" val v
        assertBool ("Took more than " ++ show maxSteps ++ " steps!") (steps <= maxSteps)

  evaluate :: Text -> IO (Either Text (Value, Int))
  evaluate = either (return . Left) evalPT . processTerm

  evalPT :: ProcessedTerm -> IO (Either Text (Value, Int))
  evalPT t = evaluateCESK (initMachine t empty emptyStore)

  evaluateCESK :: CESK -> IO (Either Text (Value, Int))
  evaluateCESK cesk = flip evalStateT (g & gameMode .~ Creative) . flip evalStateT r . runCESK 0 $ cesk
   where
    r = mkRobot "" zero zero cesk []

  runCESK :: Int -> CESK -> StateT Robot (StateT GameState IO) (Either Text (Value, Int))
  runCESK _ (Up exn _ []) = return (Left (formatExn exn))
  runCESK !steps cesk = case finalValue cesk of
    Just (v, _) -> return (Right (v, steps))
    Nothing -> stepCESK cesk >>= runCESK (steps + 1)

testModel :: TestTree
testModel =
  testGroup
    "TUI Model"
    [ testCase
        "latest repl lines at start"
        ( assertEqual
            "get 5 history [0] --> []"
            []
            (getLatestREPLHistoryItems 5 history0)
        )
    , testCase
        "latest repl lines after one input"
        ( assertEqual
            "get 5 history [0|()] --> [()]"
            [REPLEntry "()"]
            (getLatestREPLHistoryItems 5 (addREPLItem (REPLEntry "()") history0))
        )
    , testCase
        "latest repl lines after one input and output"
        ( assertEqual
            "get 5 history [0|1,1:int] --> [1,1:int]"
            [REPLEntry "1", REPLOutput "1:int"]
            (getLatestREPLHistoryItems 5 (addInOutInt 1 history0))
        )
    , testCase
        "latest repl lines after nine inputs and outputs"
        ( assertEqual
            "get 6 history [0|1,1:int .. 9,9:int] --> [7,7:int..9,9:int]"
            (concat [[REPLEntry (toT x), REPLOutput (toT x <> ":int")] | x <- [7 .. 9]])
            (getLatestREPLHistoryItems 6 (foldl (flip addInOutInt) history0 [1 .. 9]))
        )
    , testCase
        "latest repl after restart"
        ( assertEqual
            "get 5 history (restart [0|()]) --> []"
            []
            (getLatestREPLHistoryItems 5 (restartREPLHistory $ addREPLItem (REPLEntry "()") history0))
        )
    , testCase
        "current item at start"
        (assertEqual "getText [0] --> Nothing" (getCurrentItemText history0) Nothing)
    , testCase
        "current item after move to older"
        ( assertEqual
            "getText ([0]<=='') --> Just 0"
            (Just "0")
            (getCurrentItemText $ moveReplHistIndex Older "" history0)
        )
    , testCase
        "current item after move to newer"
        ( assertEqual
            "getText ([0]==>'') --> Nothing"
            Nothing
            (getCurrentItemText $ moveReplHistIndex Newer "" history0)
        )
    , testCase
        "current item after move past output"
        ( assertEqual
            "getText ([0,1,1:int]<=='') --> Just 1"
            (Just "1")
            (getCurrentItemText $ moveReplHistIndex Older "" (addInOutInt 1 history0))
        )
    , testCase
        "current item after move past same"
        ( assertEqual
            "getText ([0,1,1:int]<=='1') --> Just 0"
            (Just "0")
            (getCurrentItemText $ moveReplHistIndex Older "1" (addInOutInt 1 history0))
        )
    ]
 where
  history0 = newREPLHistory [REPLEntry "0"]
  toT :: Int -> Text
  toT = fromString . show
  addInOutInt :: Int -> REPLHistory -> REPLHistory
  addInOutInt i = addREPLItem (REPLOutput $ toT i <> ":int") . addREPLItem (REPLEntry $ toT i)
