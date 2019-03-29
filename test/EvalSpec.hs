{-# LANGUAGE OverloadedStrings #-}

module EvalSpec (spec) where

import qualified Data.Text.Lazy as T
import           Test.Hspec

import           Eval.Env
import           Eval.Eval
import           Eval.Value
import           Parser.Parser
import           Parser.Syntax
import           Pretty

spec :: Spec
spec = do
  describe "Eval" $ do
    it "number literal" $ do
      checkEval ["1"] (Number 1)

    it "bool literal" $ do
      checkEval ["true"] (Bool True)

    it "atom literal" $ do
      checkEval [":hello"] (Atom "hello")

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
          checkEval [ "length []"] (Number 0)

        it "non-empty list" $ do
          checkEval [ "length [1, 2, 3]"] (Number 3)

      it "map" $ do
        checkEval [ "map (x -> x * x) [1,2,3]"] (List [Number 1, Number 4, Number 9])

      it "foldl" $ do
        checkEval [ "foldl (acc curr -> acc - curr) 0 [1,2,3]"] (Number $ -6)

      it "foldr" $ do
        checkEval [ "foldr (acc curr -> acc - curr) 0 [1,2,3]"] (Number 2)

checkEval :: [String] -> Value -> IO ()
checkEval text expected = do
  actual <- parseAndEval (map T.pack text)
  actual `shouldBe` expected

getStmt :: T.Text -> Stmt
getStmt text =
  case parseSimple pStmt text of
    Right stmt -> stmt
    Left e     -> error (T.unpack $ ppg $ e)

parseAndEval :: [T.Text] -> IO Value
parseAndEval texts =
  let stmts = map getStmt texts
      b = Block stmts
  in do
    (Right val, _) <- runEval (evalBlock b) basicState
    return val
