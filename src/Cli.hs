module Cli
  ( cliIFace
  ) where

import           Control.Monad.State
import           Flags

import           Data.Monoid
import           Options.Applicative

data LineOpts
  = UseReplLineOpts
  | RunFileLineOpts String
  deriving (Eq, Show)

data Options = Options
  { lineOpt :: LineOpts
  , flags   :: Flags.Flags
  } deriving (Eq, Show)

withInfo :: Parser a -> String -> ParserInfo a
withInfo p desc = info (helper <*> p) $ progDesc desc

parseRepl :: Parser LineOpts
parseRepl = pure UseReplLineOpts

parseFile :: Parser LineOpts
parseFile = RunFileLineOpts <$> argument str (metavar "FILE")

parseLineOpts :: Parser LineOpts
parseLineOpts = subparser $
  command "repl" (parseRepl `withInfo` "Load repl") <>
  command "run" (parseFile `withInfo` "Run file")

parseOptions :: Parser Options
parseOptions = Options <$> parseLineOpts <*> parseFlags

krillEntry :: Options -> IO ()
krillEntry opt =
  case lineOpt opt of
    UseReplLineOpts ->
      putStrLn "repl"
    RunFileLineOpts fname ->
      putStrLn ("file: " ++ fname)

cliIFace :: IO ()
cliIFace = execParser opts >>= krillEntry
  where
    opts = info (helper <*> parseOptions)
      (  fullDesc
      <> header "The Krill Language"
      <> progDesc "Repl and iterpreter for the Krill programming language.")
