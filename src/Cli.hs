module Cli
  ( cliIFace
  ) where

import           Control.Monad.State
import           Data.Monoid
import           Data.Text.Lazy      as T
import           Data.Text.Lazy.IO   as T
import           Options.Applicative

import           Compiler
import           Flags
import           Monad
import           Pretty
import           Repl

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

runFile :: CompilerState -> FilePath -> IO ()
runFile cs fname = do
  mtext <- getFileContents fname
  let cs' = cs { _fname = Just fname, _src = mtext }

  (res, _) <- runCompilerM compileFile cs'
  case res of
    Left err  -> T.putStrLn $ ppg err
    Right ast -> return ()

krillEntry :: Options -> IO ()
krillEntry opts =
  let
    cs = emptyCS { _flags = flags opts }
  in
    case lineOpt opts of
        UseReplLineOpts ->
          repl cs
        RunFileLineOpts fname ->
          runFile cs fname

cliIFace :: IO ()
cliIFace = execParser opts >>= krillEntry
  where
    opts = info (helper <*> parseOptions)
      (  fullDesc
      <> header "The Krill Language"
      <> progDesc "Repl and iterpreter for the Krill programming language.")
