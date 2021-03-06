module Flags where

import           Options.Applicative

newtype Flags = Flags { dumpAst :: Bool}
  deriving (Eq)

flagMap :: Flags -> [(String, Bool)]
flagMap Flags
         { dumpAst = frontend
         } = [ ("ddump-ast", frontend)
             ]

instance Show Flags where
  show flags =
    foldl (\a f -> a ++ showIfNeeded f ++ "\n") "" (flagMap flags)
    where
      showIfNeeded :: (String, Bool) -> String
      showIfNeeded (s, True) = s
      showIfNeeded _         = ""

emptyFlags :: Flags
emptyFlags = Flags
  { dumpAst = False
  }

parseFlags :: Parser Flags
parseFlags = Flags
  <$> switch (  long "ddump-ast"
             <> help "Print frontend AST to console")

setFlag :: String -> Bool -> Flags -> Maybe Flags
setFlag s b flags =
  case s of
    "ddump-ast" -> Just $ flags { dumpAst = b }
    _           -> Nothing
