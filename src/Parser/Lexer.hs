{-# LANGUAGE OverloadedStrings #-}

module Parser.Lexer where

import           Control.Applicative        hiding (many, some)
import           Control.Monad              (void)
import qualified Data.Text.Lazy             as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

blockComment :: Parser ()
blockComment = empty

-- Space and newlines
scn :: Parser ()
scn = L.space space1 lineComment empty

-- Only spaces
sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing f) lineComment blockComment
  where
    f x = x == ' ' || x == '\t'

-- Consume whitespace after every lexeme (not before)
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Consume whitespace and newline after
lexemeNew :: Parser a -> Parser a
lexemeNew = L.lexeme scn

-- Parse a fixed string
symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

-- Parse something between parenthesis
parens :: Parser a -> Parser a
parens = between (lexeme $ char '(') (lexeme $ char ')')

-- Parse something between braces
braces :: Parser a -> Parser a
braces = between (lexemeNew $ char '{') (lexemeNew $ char '}')

-- Parse an integer
integer :: Parser Integer
integer = lexeme L.decimal

-- Parse a double
double :: Parser Double
double = lexeme L.float

-- Common symbols

comma, colon, equals, implies, pipe, dcolon, quote, dquote, bslash, arrow, unit,
  lbracket, rbracket
  :: Parser ()

arrow = void $ lexeme $ symbol "->"
bslash = void $ lexeme $ char '\\'
colon = void $ lexeme $ symbol ":"
comma = void $ lexeme $ char ','
dcolon = void $ lexeme $ symbol "::"
dquote = void $ lexeme $ char '"'
equals = void $ lexeme $ char '='
implies = void $ lexeme $ symbol "=>"
pipe = void $ lexeme $ char '|'
quote = void $ lexeme $ char '\''
unit = void $ lexeme $ symbol "()"
lbracket = void $ lexeme $ char '['
rbracket = void $ lexeme $ char ']'

-- Parse an escaped character
escapedChars :: Parser Char
escapedChars = do
  _ <- char '\\'                   -- a backslash
  x <- oneOf ("\\\"nrt" :: String) -- either backslash or doublequote
  return $ case x of
    '\\' -> x
    '"'  -> x
    'n'  -> '\n'
    'r'  -> '\r'
    't'  -> '\t'
    _    -> x

-- List of reserved words
reservedWords :: [String]
reservedWords =
  [ "if"
  , "then"
  , "else"
  , "module"
  , "true"
  , "false"
  , "for"
  , "in"
  ]

-- Parse a reserved word
rword :: String -> Parser ()
rword w = (lexeme . try) (string (T.pack w) *> notFollowedBy alphaNumChar)

idCheck :: Parser String
idCheck = (p >>= check)
  where
    p = (:) <$> (oneOf $ letterCharUnder) <*> many (oneOf $ '_' : letterCharUnder ++ ['0'..'9'])
    letterCharUnder = '_' : ['a'..'z'] ++ ['A'..'Z']
    check x = if x `elem` reservedWords
                 then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                 else return x

-- Parse an identifier and consume whitespace after
identifier :: Parser String
identifier = (lexeme . try) idCheck

idn :: Parser String
idn = idCheck

-- Parse an identifier that passes a predicate
predIdentifier :: (String -> Bool) -> String -> Parser String
predIdentifier p err = identifier >>= check
  where
    check x = if p x
      then return x
      else fail $ "identifier " ++ x ++ " " ++ err

-- Parse an uppercase identifier
upperIdentifier :: Parser String
upperIdentifier = predIdentifier (p . head) "does not start with a uppercase letter"
  where
    p = flip elem ('_' : ['A'..'Z'])

-- Parse a lowercase identifier
lowerIdentifier :: Parser String
lowerIdentifier = predIdentifier (p . head) "does not start with a lowercase letter"
  where
    p = flip elem ('_' : ['a'..'z'])

-- Convert char in Parser to string
pCharToString :: Parser Char -> Parser T.Text
pCharToString pc = do
  c <- pc
  return $ T.pack $ show c

-- String parser to Text
pText :: Parser String -> Parser T.Text
pText p = do
  s <- p
  return $ T.pack s
