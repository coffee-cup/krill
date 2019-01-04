module Parser where

import qualified Data.Text.Lazy             as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Error

import           Lexer
import           Syntax

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

pLiteral :: Parser Literal
pLiteral = pNumberLit
  <|> pCharLit
  <|> pStringLit
  <|> pBoolLit

contents :: Parser a -> Parser a
contents p = do
  r <- lexeme p
  eof
  return r

parseUnpack :: Either (ParseErrorBundle T.Text Void) a -> Either String a
parseUnpack res = case res of
  Right a  -> Right a
  Left err -> Left $ errorBundlePretty err

parseSimple :: Parser a -> T.Text -> Either String a
parseSimple p = parseUnpack . runParser (contents p) "<stdin>" . T.strip

parseSimpleString :: Parser a -> String -> Either String a
parseSimpleString p = parseSimple p . T.pack
