{-# LANGUAGE OverloadedStrings #-}

module SyntaxSpec (spec) where

import           Test.Hspec

import           Parser.Syntax

spec :: Spec
spec = do
  describe "Helpers" $ do
    it "makes EApp with 2 args" $ do
      mkEApp [EVar NoLoc "a", EVar NoLoc "b"] `shouldBe` EApp NoLoc (EVar NoLoc "a") (EVar NoLoc "b")

    it "makes EApp with 3 args" $ do
      mkEApp [EVar NoLoc "a", EVar NoLoc "b", EVar NoLoc "c"] `shouldBe` EApp NoLoc (EApp NoLoc (EVar NoLoc "a") (EVar NoLoc "b")) (EVar NoLoc "c")
