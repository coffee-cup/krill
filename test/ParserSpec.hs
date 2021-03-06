{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import           Data.Either
import qualified Data.Text.Lazy as T
import           Test.Hspec
-- import           Test.Hspec.Expectations.Contrib

import           Parser.Lexer
import           Parser.Parser
import           Parser.Syntax

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

    it "parses unit" $ do
      parseSimple pLiteral "()" `shouldBe` (Right $ LitUnit)

  describe "Expressions" $ do
    it "parses expression literal" $ do
      parseSimple pExpr "3.2" `shouldBe` (Right $ ELit NoLoc (LitNumber 3.2))

    it "parses expression literal unit" $ do
      parseSimple pExpr "()" `shouldBe` (Right $ ELit NoLoc LitUnit)

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

    describe "Assignment" $ do
      it "simple assignment" $
        parseSimple pExpr "a = b" `shouldBe` (Right $ EAss NoLoc "a" (EVar NoLoc "b"))


    describe "Lambda" $ do
      it "simple lambda" $
        parseSimple pExpr "x y -> x" `shouldBe` (Right $ ELam NoLoc ["x", "y"]
                                                (Block [SExpr NoLoc (EVar NoLoc "x")]))

    describe "If" $ do
      it "simple if" $
         parseSimple pExpr "if x then y else z" `shouldBe` (Right $ EIf NoLoc
                                                           (EVar NoLoc "x")
                                                           (Block [SExpr NoLoc (EVar NoLoc "y")])
                                                           (Block [SExpr NoLoc (EVar NoLoc "z")]))

    describe "For" $ do
      it "simple for" $
        parseSimple pExpr "for i in [1] { i }" `shouldBe` (Right $ EFor NoLoc
                                                          "i"
                                                          (EList NoLoc [ELit NoLoc $ LitNumber 1])
                                                          (Block [SExpr NoLoc (EVar NoLoc "i")]))

    describe "List" $ do
      it "empty list" $
        parseSimple pExpr "[]" `shouldBe` (Right $ EList NoLoc [])

      it "list with items" $
        parseSimple pExpr "[1, a]" `shouldBe` (Right $ EList NoLoc
                                              [ ELit NoLoc $ LitNumber 1
                                              , EVar NoLoc "a"])


    describe "List Access" $ do
      it "accesses list" $
        parseSimple pExpr "a[1]" `shouldBe` (Right $ EListAcc NoLoc
                                            "a" (ELit NoLoc $ LitNumber 1))

      it "should not access list" $
        parseSimple pExpr "a [1]" `shouldBe` (Right $ EApp NoLoc
                                               (EVar NoLoc "a")
                                               (EList NoLoc [ELit NoLoc $ LitNumber 1]))

    describe "List Range" $ do
      it "range with no next" $
        parseSimple pExpr "[1..x]" `shouldBe` (Right $ ERange NoLoc
                                               (ELit NoLoc $ LitNumber 1)
                                               (ELit NoLoc $ LitUnit)
                                               (EVar NoLoc "x"))

      it "range with next" $
        parseSimple pExpr "[1,2..x]" `shouldBe` (Right $ ERange NoLoc
                                               (ELit NoLoc $ LitNumber 1)
                                               (ELit NoLoc $ LitNumber 2)
                                               (EVar NoLoc "x"))

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
