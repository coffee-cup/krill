module Parser where

import           Control.Monad.Combinators.Expr
import qualified Data.Text.Lazy                 as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Megaparsec.Error

import           Lexer
import           Syntax

pName :: Parser Name
pName = pText identifier

-- Literal Parsers

pNumberLit :: Parser Literal
pNumberLit = LitNumber <$> p <?> "number"
  where
    p = try double <|> pInt
    pInt = do
      i <- integer
      return $ fromInteger i

pCharLit :: Parser Literal
pCharLit = do
  quote
  c <- anySingle
  quote
  (return $ LitChar c) <?> "char"

pStringLit :: Parser Literal
pStringLit = do
  dquote
  x <- many $ escapedChars <|> noneOf ("\"\\" :: String)
  dquote
  (return $ LitString $ T.pack x) <?> "string"

pBoolLit :: Parser Literal
pBoolLit = p <?> "boolean"
  where
    p = (rword "true" >> return (LitBool True))
      <|> (rword "false" >> return (LitBool False))

pAtomLit :: Parser Literal
pAtomLit = p <?> "atom"
  where
    p = do
      colon
      x <- pName
      return $ LitAtom x

pLiteral :: Parser Literal
pLiteral = pNumberLit
  <|> pCharLit
  <|> pStringLit
  <|> pBoolLit
  <|> pAtomLit

-- Expressions

pExprLiteral :: Parser Expr
pExprLiteral = ELit <$> pLiteral <?> "literal"

pExprVar :: Parser Expr
pExprVar = EVar <$> pName

mkPrefix :: T.Text -> Expr -> Expr
mkPrefix name = EUnOp name

mkBinary :: T.Text -> Expr -> Expr -> Expr
mkBinary name = EBinOp name

prefix :: T.Text -> Operator Parser Expr
prefix name = Prefix (mkPrefix <$> symbol name)

binary :: T.Text -> Operator Parser Expr
binary name = InfixL (mkBinary <$> symbol name)

operators :: [[Operator Parser Expr]]
operators =
  [ [ prefix "-"
    , prefix "!" ]
  , [ binary "*"
    , binary "/" ]
  , [ binary "+"
    , binary "-" ]
  , [ binary "=="
    , binary "<="
    , binary ">="
    , binary "<"
    , binary ">" ]
  , [ binary "&&" ]
  , [ binary "||" ] ]

pExprParens :: Parser Expr
pExprParens = EParens <$> parens pExpr <?> "parens"

aexpr :: Parser Expr
aexpr = do
  r <- some $ choice [ pExprParens
                     , pExprLiteral
                     , pExprVar
                     ]
  return $ Prelude.foldl1 EApp r

pExpr :: Parser Expr
pExpr = makeExprParser aexpr operators

-- Statements

pStmtExpr :: Parser Stmt
pStmtExpr = do
  e <- pExpr
  try scn
  return $ SExpr e

pStmtAss :: Parser Stmt
pStmtAss = do
  n <- pName
  equals
  e <- pExpr
  try scn
  return $ SAss n e

pStmt :: Parser Stmt
pStmt = try pStmtAss
  <|> pStmtExpr

pBlock :: Parser Block
pBlock = pMultiLine <|> pSingleLine
  where
    pSingleLine = do
      s <- pStmt
      return $ Block [s]
    pMultiLine = do
      ss <- (braces . many) (pStmt <* try scn)
      return $ Block ss

-- Declarations

pFuncDecl :: Parser Decl
pFuncDecl = do
  n <- pName
  args <- many pName
  arrow
  Func n args <$> pBlock

pDecl :: Parser Decl
pDecl = pFuncDecl

-- Module

pModule :: Parser Module
pModule = Module <$> many pDecl

contents :: Parser a -> Parser a
contents p = do
  r <- lexeme p
  eof
  return r

parseUnpack :: Either (ParseErrorBundle T.Text Void) a -> Either String a
parseUnpack res = case res of
  Right a  -> Right a
  Left err -> Left $ errorBundlePretty err

parseModule :: T.Text -> T.Text -> Either String Module
parseModule input = runKrillParser input pModule

parseSimple :: Parser a -> T.Text -> Either String a
parseSimple p = parseUnpack . runParser (contents p) "<stdin>" . T.strip

parseSimpleString :: Parser a -> String -> Either String a
parseSimpleString p = parseSimple p . T.pack

runKrillParser :: T.Text -> Parser a -> T.Text -> Either String a
runKrillParser input p =
  parseUnpack . runParser (contents p) (T.unpack input) . T.strip
