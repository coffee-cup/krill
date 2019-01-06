{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import           Data.Either
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
      parseSimple pExpr "4 # test" `shouldBe` (Right $ (ELit (LitNumber 4.0)))

  describe "Name" $ do
    it "parses valid name" $ do
      parseSimple pName "ah_4Ff" `shouldBe` Right "ah_4Ff"

  describe "Literals" $ do
    it "parses integer" $ do
      parseSimple pLiteral "4" `shouldBe` (Right $ LitNumber 4.0)

    it "parses double" $ do
      parseSimple pLiteral "4.4" `shouldBe` (Right $ LitNumber 4.4)

    it "parses double with leading 0" $ do
      parseSimple pLiteral "0.1" `shouldBe` (Right $ LitNumber 0.1)

    it "parses character" $ do
      parseSimple pLiteral "'a'" `shouldBe` (Right $ LitChar 'a')

    it "parses invalid character" $ do
      isLeft $ parseSimple pLiteral "'ab'"

    it "parses string" $ do
      parseSimple pLiteral "\"hello\"" `shouldBe` (Right $ LitString "hello")

    it "parses bool true" $ do
      parseSimple pLiteral "true" `shouldBe` (Right $ LitBool True)

    it "parses bool false" $ do
      parseSimple pLiteral "false" `shouldBe` (Right $ LitBool False)

    it "parses atom" $ do
      parseSimple pLiteral ":atom" `shouldBe` (Right $ LitAtom "atom")

  describe "Expressions" $ do
    it "parses expression literal" $ do
      parseSimple pExpr "3.2" `shouldBe` (Right $ ELit (LitNumber 3.2))

    describe "Variables" $ do
      it "lowercase simple var" $
        parseSimple pExpr "a" `shouldBe` (Right $ EVar "a")

      it "underscore var" $
        parseSimple pExpr "_" `shouldBe` (Right $ EVar "_")

      it "invalid identifier" $
        isLeft $ parseSimple pExpr "#(&)%)#(&%#*(^%))1234"

    describe "Parens" $ do
      it "parens literal" $
        parseSimple pExpr "(3)" `shouldBe` (Right $ EParens $ ELit $ LitNumber 3)

      it "parens arithmetic" $
        parseSimple pExpr "(a + 3)" `shouldBe` (Right $ EParens $
                                               (EBinOp "+" (EVar "a"))
                                               (ELit $ LitNumber 3))

    describe "Application" $ do
      it "single application" $
        parseSimple pExpr "x y" `shouldBe` (Right $ EApp (EVar "x") (EVar "y"))

      it "multiple application" $
        parseSimple pExpr "x y z" `shouldBe` (Right $ EApp
                                             (EApp (EVar "x") (EVar "y"))
                                             (EVar "z"))

    -- describe "Lambda" $ do
    --   it "single line, single param" $
    --     parseSimple pExpr "\\x -> y" `shouldBe` (Right $ ELam
    --                                            [Name "x"] [EVar $ Name "y"])

    --   it "single line, multi param" $
    --     parseSimple pExpr "\\x y -> x" `shouldBe` (Right $ ELam
    --                                               [Name "x", Name "y"] [EVar $ Name "x"])

    --   it "multi line, multi param" $
    --     parseSimpleUnlines pExpr ["\\x y ->", "  1", "  y"] `shouldBe` (Right $ ELam
    --                                                                    [Name "x", Name "y"]
    --                                                                    [ELit $ LitInt 1, EVar $ Name "y"])

    describe "Operators" $ do
      it "unary negation" $
        parseSimple pExpr "-a" `shouldBe` (Right $ EUnOp "-" (EVar "a"))

      it "unary not" $
        parseSimple pExpr "!a" `shouldBe` (Right $ EUnOp "!" (EVar "a"))

      it "addition" $
        parseSimple pExpr "1 + 2" `shouldBe` (Right $
                                              (EBinOp "+"
                                               (ELit $ LitNumber 1)
                                               (ELit $ LitNumber 2)))

      it "subtraction" $
        parseSimple pExpr "1 - 2" `shouldBe` (Right $
                                             (EBinOp "-"
                                               (ELit $ LitNumber 1))
                                               (ELit $ LitNumber 2))

      it "multiplication" $
        parseSimple pExpr "1 * 2" `shouldBe` (Right $
                                             (EBinOp "*"
                                               (ELit $ LitNumber 1))
                                               (ELit $ LitNumber 2))

      it "division" $
        parseSimple pExpr "1 / 2" `shouldBe` (Right $
                                             (EBinOp "/"
                                               (ELit $ LitNumber 1))
                                               (ELit $ LitNumber 2))

      it "equal" $
        parseSimple pExpr "1 == 2" `shouldBe` (Right $
                                              (EBinOp "=="
                                                (ELit $ LitNumber 1))
                                                (ELit $ LitNumber 2))

      it "less than" $
        parseSimple pExpr "1 < 2" `shouldBe` (Right $
                                             (EBinOp "<"
                                               (ELit $ LitNumber 1))
                                               (ELit $ LitNumber 2))

      it "greater than" $
        parseSimple pExpr "1 > 2" `shouldBe` (Right $
                                             (EBinOp ">"
                                               (ELit $ LitNumber 1))
                                               (ELit $ LitNumber 2))

      it "less than equal" $
        parseSimple pExpr "1 <= 2" `shouldBe` (Right $
                                              (EBinOp "<="
                                                (ELit $ LitNumber 1))
                                                (ELit $ LitNumber 2))

      it "greater than equal" $
        parseSimple pExpr "1 >= 2" `shouldBe` (Right $
                                              (EBinOp ">="
                                                (ELit $ LitNumber 1))
                                                (ELit $ LitNumber 2))

      it "and" $
        parseSimple pExpr "false && true" `shouldBe` (Right $
                                                     (EBinOp "&&"
                                                       (ELit $ LitBool False))
                                                       (ELit $ LitBool True))

      it "or" $
        parseSimple pExpr "false || true" `shouldBe` (Right $
                                                     (EBinOp "||"
                                                       (ELit $ LitBool False))
                                                       (ELit $ LitBool True))
      it "arithmetic presedence" $
        parseSimple pExpr "4 * -3 - 2 / 5" `shouldBe` (Right $
                                                       (EBinOp "-"
                                                        (EBinOp "*"
                                                         (ELit (LitNumber 4.0))
                                                         (EUnOp "-" (ELit (LitNumber 3.0))))
                                                         (EBinOp "/"
                                                          (ELit (LitNumber 2.0))
                                                          (ELit (LitNumber 5.0)))))

      it "binary presedence" $
        parseSimple pExpr "false && !true || false" `shouldBe` (Right $
                                                               (EBinOp "||"
                                                                 ((EBinOp "&&"
                                                                   (ELit $ LitBool False))
                                                                   (EUnOp "!"
                                                                    (ELit $ LitBool True)))
                                                                 (ELit $ LitBool False)))

parseSimpleUnlines :: Parser a -> [String] -> Either String a
parseSimpleUnlines p =
  parseSimple p . T.pack . unlines
