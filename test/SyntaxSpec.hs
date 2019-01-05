{-# LANGUAGE OverloadedStrings #-}

module SyntaxSpec (spec) where

import qualified Data.Text.Lazy as T
import           Test.Hspec

import           Syntax

spec :: Spec
spec = do
  describe "Helpers" $ do
    it "makes EApp" $ do
      mkEApp [EVar "a", EVar "b"] `shouldBe` EApp (EVar "a") (EVar "b")
