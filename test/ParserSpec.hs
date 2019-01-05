{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import qualified Data.Text.Lazy as T
import           Test.Hspec
-- import           Test.Hspec.Expectations.Contrib

import           Lexer
import           Parser
import           Syntax

spec :: Spec
spec = do
  describe "Comments" $ do
    it "ignore comment" $ do
      parseSimpleString pExpr "4 # test" `shouldBe` (Right $ (ELit (LitNumber 4.0)))

parseSimpleUnlines :: Parser a -> [String] -> Either String a
parseSimpleUnlines p =
  parseSimple p . T.pack . unlines
