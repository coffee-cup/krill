module Parser.Parser where

import           Control.Monad.Combinators.Expr
import qualified Data.Text.Lazy                 as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Megaparsec.Error

import           Parser.Lexer
import           Parser.Syntax

pName :: Parser Name
pName = pText identifier

pId :: Parser Name
pId = pText idn

getLoc :: Parser Loc
getLoc = Located <$> getSourcePos

-- Literals

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
  return (LitChar c) <?> "char"

pStringLit :: Parser Literal
pStringLit = do
  dquote
  x <- many $ escapedChars <|> noneOf ("\"\\" :: String)
  dquote
  return (LitString $ T.pack x) <?> "string"

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
      LitAtom <$> pName

pUnitLit :: Parser Literal
pUnitLit = p <?> "unit"
  where
    p = unit *> return LitUnit

pLiteral :: Parser Literal
pLiteral = pNumberLit
  <|> pCharLit
  <|> pStringLit
  <|> pBoolLit
  <|> pAtomLit
  <|> pUnitLit

-- Expressions

pExprLiteral :: Parser Expr
pExprLiteral = ELit <$> getLoc <*> pLiteral <?> "literal"

pExprVar :: Parser Expr
pExprVar = EVar <$> getLoc <*> pName

mkPrefix :: Loc -> T.Text -> Expr -> Expr
mkPrefix loc name = EUnOp loc name

mkBinary :: Loc -> T.Text -> Expr -> Expr -> Expr
mkBinary loc name = EBinOp loc name

prefix :: T.Text -> Operator Parser Expr
prefix name = Prefix (mkPrefix <$> getLoc <*> symbol name <?> "unary operator")

binary :: T.Text -> Parser (Expr -> Expr -> Expr)
binary name = mkBinary <$> getLoc <*> symbol name <?> "binary operator"

bn, bl, br :: T.Text -> Operator Parser Expr
br = InfixR . binary
bl = InfixL . binary
bn = InfixN . binary

binaryr :: T.Text -> Operator Parser Expr
binaryr name = InfixR (mkBinary <$> getLoc <*> symbol name <?> "binary operator")

operators :: [[Operator Parser Expr]]
operators =
  [ [ prefix "-"
    , prefix "!" ]
  , [ bl "*"
    , bl "/" ]
  , [ bl "+"
    , bl "-" ]
  , [ bn "=="
    , bn "<="
    , bn ">="
    , bn "<"
    , bn ">" ]
  , [ br "&&" ]
  , [ br "||" ] ]

pExprLam :: Parser Expr
pExprLam = do
  l <- getLoc
  args <- many pName
  arrow
  ELam l args <$> pBlock

pExprIf :: Parser Expr
pExprIf = do
  l <- getLoc
  rword "if"
  cond <- pExpr
  rword "then"
  thenBlock <- pBlock
  rword "else"
  elseBlock <- pBlock
  return $ EIf l cond thenBlock elseBlock

pExprFor :: Parser Expr
pExprFor = do
  l <- getLoc
  rword "for"
  name <- pName
  rword "in"
  e <- pExpr
  b <- pBlock
  return $ EFor l name e b

pExprAss :: Parser Expr
pExprAss = do
  l <- getLoc
  n <- pName
  equals
  e <- pExpr
  try scn
  return $ EAss l n e

pExprList :: Parser Expr
pExprList = do
  l <- getLoc
  lbracket
  xs <- pExpr `sepBy` comma
  rbracket
  return $ EList l xs

pExprListAcc :: Parser Expr
pExprListAcc = do
  l <- getLoc
  n <- pId
  lbracket
  e <- pExpr
  rbracket
  return $ EListAcc l n e

pExprParens :: Parser Expr
pExprParens = EParens <$> getLoc <*> parens pExpr <?> "parens"

aexpr :: Parser Expr
aexpr = do
  r <- some $ choice [ try pExprLiteral
                     , pExprParens
                     , pExprList
                     , try pExprListAcc
                     , try pExprVar
                     ]
  l <- getLoc
  return $ Prelude.foldl1 (EApp l) r

pExpr :: Parser Expr
pExpr = try pExprLam
  <|> pExprIf
  <|> pExprFor
  <|> try pExprAss
  <|> makeExprParser aexpr operators

-- Statements

pStmtExpr :: Parser Stmt
pStmtExpr = (SExpr <$> getLoc <*> pExpr) <* try scn

pStmt :: Parser Stmt
pStmt = pStmtExpr

-- Blocks

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
  loc <- getLoc
  n <- pName
  args <- many pName
  arrow
  Func loc n args <$> pBlock

pDecl :: Parser Decl
pDecl = pFuncDecl

-- Module

pModule :: Parser Module
pModule = Module <$> many pStmt

contents :: Parser a -> Parser a
contents p = lexeme p <* eof

parseUnpack :: Either (ParseErrorBundle T.Text Void) a -> Either String a
parseUnpack res = case res of
  Right a  -> Right a
  Left err -> Left $ errorBundlePretty err

parseModule :: T.Text -> T.Text -> Either String Module
parseModule input = runKrillParser input pModule

parseStmt :: T.Text -> T.Text -> Either String Stmt
parseStmt input = runKrillParser input pStmt

parseSimple :: Parser a -> T.Text -> Either String a
parseSimple p = parseUnpack . runParser (contents p) "<stdin>" . T.strip

parseSimpleString :: Parser a -> String -> Either String a
parseSimpleString p = parseSimple p . T.pack

runKrillParser :: T.Text -> Parser a -> T.Text -> Either String a
runKrillParser input p =
  parseUnpack . runParser (contents p) (T.unpack input) . T.strip

