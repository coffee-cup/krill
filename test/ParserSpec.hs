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
      parseSimple pExpr "4 # test" `shouldBe` (Right $ (ELit NoLoc (LitNumber 4.0)))

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
      parseSimple pExpr "3.2" `shouldBe` (Right $ ELit NoLoc (LitNumber 3.2))

    describe "Variables" $ do
      it "lowercase simple var" $
        parseSimple pExpr "a" `shouldBe` (Right $ EVar NoLoc "a")

      it "underscore var" $
        parseSimple pExpr "_" `shouldBe` (Right $ EVar NoLoc "_")

      it "invalid identifier" $
        isLeft $ parseSimple pExpr "#(&)%)#(&%#*(^%))1234"

    describe "Parens" $ do
      it "parens literal" $
        parseSimple pExpr "(3)" `shouldBe` (Right $ EParens NoLoc $ ELit NoLoc $ LitNumber 3)

      it "parens with spaces" $
        parseSimple pExpr "( 3 )" `shouldBe` (Right $ EParens NoLoc $ ELit NoLoc $ LitNumber 3)

      it "parens arithmetic" $
        parseSimple pExpr "(a + 3)" `shouldBe` (Right $ EParens NoLoc $
                                               (EBinOp NoLoc "+" (EVar NoLoc "a"))
                                               (ELit NoLoc $ LitNumber 3))

    describe "Application" $ do
      it "single application" $
        parseSimple pExpr "x y" `shouldBe` (Right $ EApp NoLoc (EVar NoLoc "x") (EVar NoLoc "y"))

      it "multiple application" $
        parseSimple pExpr "x y z" `shouldBe` (Right $ EApp NoLoc
                                             (EApp NoLoc (EVar NoLoc "x") (EVar NoLoc "y"))
                                             (EVar NoLoc "z"))

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
        parseSimple pExpr "-a" `shouldBe` (Right $ EUnOp NoLoc "-" (EVar NoLoc "a"))

      it "unary not" $
        parseSimple pExpr "!a" `shouldBe` (Right $ EUnOp NoLoc "!" (EVar NoLoc "a"))

      it "addition" $
        parseSimple pExpr "1 + 2" `shouldBe` (Right $
                                              (EBinOp NoLoc "+"
                                               (ELit NoLoc $ LitNumber 1)
                                               (ELit NoLoc $ LitNumber 2)))

      it "subtraction" $
        parseSimple pExpr "1 - 2" `shouldBe` (Right $
                                             (EBinOp NoLoc "-"
                                               (ELit NoLoc $ LitNumber 1))
                                               (ELit NoLoc $ LitNumber 2))

      it "multiplication" $
        parseSimple pExpr "1 * 2" `shouldBe` (Right $
                                             (EBinOp NoLoc "*"
                                               (ELit NoLoc $ LitNumber 1))
                                               (ELit NoLoc $ LitNumber 2))

      it "division" $
        parseSimple pExpr "1 / 2" `shouldBe` (Right $
                                             (EBinOp NoLoc "/"
                                               (ELit NoLoc $ LitNumber 1))
                                               (ELit NoLoc $ LitNumber 2))

      it "equal" $
        parseSimple pExpr "1 == 2" `shouldBe` (Right $
                                              (EBinOp NoLoc "=="
                                                (ELit NoLoc $ LitNumber 1))
                                                (ELit NoLoc $ LitNumber 2))

      it "less than" $
        parseSimple pExpr "1 < 2" `shouldBe` (Right $
                                             (EBinOp NoLoc "<"
                                               (ELit NoLoc $ LitNumber 1))
                                               (ELit NoLoc $ LitNumber 2))

      it "greater than" $
        parseSimple pExpr "1 > 2" `shouldBe` (Right $
                                             (EBinOp NoLoc ">"
                                               (ELit NoLoc $ LitNumber 1))
                                               (ELit NoLoc $ LitNumber 2))

      it "less than equal" $
        parseSimple pExpr "1 <= 2" `shouldBe` (Right $
                                              (EBinOp NoLoc "<="
                                                (ELit NoLoc $ LitNumber 1))
                                                (ELit NoLoc $ LitNumber 2))

      it "greater than equal" $
        parseSimple pExpr "1 >= 2" `shouldBe` (Right $
                                              (EBinOp NoLoc ">="
                                                (ELit NoLoc $ LitNumber 1))
                                                (ELit NoLoc $ LitNumber 2))

      it "and" $
        parseSimple pExpr "false && true" `shouldBe` (Right $
                                                     (EBinOp NoLoc "&&"
                                                       (ELit NoLoc $ LitBool False))
                                                       (ELit NoLoc $ LitBool True))

      it "or" $
        parseSimple pExpr "false || true" `shouldBe` (Right $
                                                     (EBinOp NoLoc "||"
                                                       (ELit NoLoc $ LitBool False))
                                                       (ELit NoLoc $ LitBool True))
      it "arithmetic presedence" $
        parseSimple pExpr "4 * -3 - 2 / 5" `shouldBe` (Right $
                                                       (EBinOp NoLoc "-"
                                                        (EBinOp NoLoc "*"
                                                         (ELit NoLoc (LitNumber 4.0))
                                                         (EUnOp NoLoc "-" (ELit NoLoc (LitNumber 3.0))))
                                                         (EBinOp NoLoc "/"
                                                          (ELit NoLoc (LitNumber 2.0))
                                                          (ELit NoLoc (LitNumber 5.0)))))

      it "binary presedence" $
        parseSimple pExpr "false && !true || false" `shouldBe` (Right $
                                                               (EBinOp NoLoc "||"
                                                                 ((EBinOp NoLoc "&&"
                                                                   (ELit NoLoc $ LitBool False))
                                                                   (EUnOp NoLoc "!"
                                                                    (ELit NoLoc $ LitBool True)))
                                                                 (ELit NoLoc $ LitBool False)))

  describe "Statements" $ do
    it "parses statement expression" $
      parseSimple pStmt "a" `shouldBe` (Right $ SExpr NoLoc $ EVar NoLoc "a")

    it "parses assignment" $
      parseSimple pStmt "a = b" `shouldBe` (Right $ SAss NoLoc "a" (EVar NoLoc "b"))

  describe "Blocks" $ do
    it "parses single line block" $
      parseSimple pBlock "a" `shouldBe` (Right $ Block [SExpr NoLoc $ EVar NoLoc "a"])

    it "parses multiline block" $
      parseSimpleUnlines pBlock ["{", "  a", "b", "}"] `shouldBe` (Right $ Block
                                                                  [ SExpr NoLoc $ EVar NoLoc "a"
                                                                  , SExpr NoLoc $ EVar NoLoc "b"
                                                                  ])


parseSimpleUnlines :: Parser a -> [String] -> Either String a
parseSimpleUnlines p =
  parseSimple p . T.pack . unlines
