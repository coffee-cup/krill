{-# LANGUAGE OverloadedStrings #-}

module SyntaxSpec (spec) where

import qualified Data.Text.Lazy as T
import           Test.Hspec

import           Syntax

spec :: Spec
spec = do
  describe "Helpers" $ do
    it "makes EApp with 2 args" $ do
      mkEApp [EVar NoLoc "a", EVar NoLoc "b"] `shouldBe` EApp NoLoc (EVar NoLoc "a") (EVar NoLoc "b")

    it "makes EApp with 3 args" $ do
      mkEApp [EVar NoLoc "a", EVar NoLoc "b", EVar NoLoc "c"] `shouldBe` EApp NoLoc (EApp NoLoc (EVar NoLoc "a") (EVar NoLoc "b")) (EVar NoLoc "c")
