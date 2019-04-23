{-# LANGUAGE OverloadedStrings #-}

module EvalSpec (spec) where

import qualified Data.Text.Lazy as T
import           Test.Hspec

import           Eval.Eval
import           Eval.Value
import           Parser.Parser
import           Parser.Syntax
import           Pretty

spec :: Spec
spec = do
  describe "Eval" $ do
    describe "Literals" $ do
      it "number literal" $ do
        checkEval ["1"] (Number 1)

      it "bool literal" $ do
        checkEval ["true"] (Bool True)

      it "atom literal" $ do
        checkEval [":hello"] (Atom "hello")

    describe "Operators" $ do
      describe "chain" $ do
        it "fail without chain" $ do
          checkError ["length map (x -> x * x) [1,3]"]

        it "chain" $ do
          checkEval ["length $ map (x -> x * x) [1,3]"] (Number 2)

      describe "concat" $ do
        it "lists" $ do
          checkEval ["[1,2] ++ [3,4]"] (List [Number 1, Number 2, Number 3, Number 4])

        it "strings" $ do
          checkEval ["\"a \" ++ \" b\""] (String "a  b")

        it "string to number" $ do
          checkEval ["\"a \" ++ 1"] (String "a 1")

    describe "Range" $ do
      it "simple range" $ do
        checkEval ["[1..5]"] (List [Number 1, Number 2, Number 3, Number 4, Number 5])

      it "range with next" $ do
        checkEval ["[1,3..5]"] (List [Number 1, Number 3, Number 5])

      it "variable in range" $ do
        checkEval ["x = 5", "[1,3..x]"] (List [Number 1, Number 3, Number 5])

    it "variable lookup" $ do
      checkEval ["x = 1", "x"] (Number 1)

    it "function call" $ do
      checkEval ["foo = x y -> x + y", "foo 2 3"] (Number 5)

    it "if true branch" $ do
      checkEval [ "x = true"
                , "if x then 1 else 2"] (Number 1)

    it "if false branch" $ do
      checkEval [ "x = 1"
                , "if x == 2 then 1 else 2"] (Number 2)

    it "recursive function" $ do
      checkEval [ "foo = n -> if n == 0 then :hello else foo (n - 1)"
                , "foo 10"] (Atom "hello")

    it "binding" $ do
      checkEval [ "x = 1"
                , "foo = x -> x"
                , "foo 2" ] (Number 2)

    it "scopes 1" $ do
      checkEval [ "x = 1"
                , unlines ["foo = a -> {"
                , "  x = 2"
                , "  bar = b -> {"
                , "    x = 3"
                , "    x"
                , "  }"
                , "  bar 0"
                , "}"]
                , "foo 0"] (Number 3)

    describe "Builtins" $ do
      describe "length" $ do
        it "empty list" $ do
          checkEval ["length []"] (Number 0)

        it "non-empty list" $ do
          checkEval ["length [1, 2, 3]"] (Number 3)

      it "map" $ do
        checkEval ["map (x -> x * x) [1,2,3]"] (List [Number 1, Number 4, Number 9])

      it "foldl" $ do
        checkEval ["foldl (acc curr -> acc - curr) 0 [1,2,3]"] (Number $ -6)

      it "foldr" $ do
        checkEval ["foldr (acc curr -> acc - curr) 0 [1,2,3]"] (Number 2)

      describe "toNumber" $ do
        it "parses int to number" $ do
          checkEval ["toNumber \"2\""] (Number 2)

        it "parses double to number" $ do
          checkEval ["toNumber \"3.14\""] (Number 3.14)

        it "fails to parse no-number" $ do
          checkError ["toNumber \"a\""]

checkEval :: [String] -> Value -> IO ()
checkEval text expected = do
  actual <- parseAndEval (map T.pack text)
  actual `shouldBe` expected

getStmt :: T.Text -> Stmt
getStmt text =
  case parseSimple pStmt text of
    Right stmt -> stmt
    Left e     -> error (T.unpack $ ppg $ e)

checkError :: [String] -> IO ()
checkError text =
  let texts = map T.pack text
      stmts = map getStmt texts
      b = Block stmts
  in do
    res <- runEval (evalBlock b) basicState
    case res of
      (Left _, _)  -> return ()
      (Right _, _) -> "no error" `shouldBe` "error"

parseAndEval :: [T.Text] -> IO Value
parseAndEval texts =
  let stmts = map getStmt texts
      b = Block stmts
  in do
    (Right val, _) <- runEval (evalBlock b) basicState
    return val
