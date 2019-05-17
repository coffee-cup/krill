module Cli
  ( cliIFace
  ) where

import           Control.Applicative
import           Data.Text.Lazy.IO   as T
import           Options.Applicative

import           Compiler
import           Flags
import           Monad
import           Pretty
import           Repl
import           System.Exit

data LineOpts
  = UseReplLineOpts
  | RunFileLineOpts [String]
  deriving (Eq, Show)

data Options = Options
  { lineOpt :: LineOpts
  , flags   :: Flags.Flags
  } deriving (Eq, Show)

parseRepl :: Parser LineOpts
parseRepl = pure UseReplLineOpts

manyStr :: Parser [String]
manyStr = some (strArgument (metavar "file?" <> help "File to run. Start repl if no file provided."))

parseFile :: Parser LineOpts
parseFile = RunFileLineOpts <$> manyStr

parseLineOpts :: Parser LineOpts
parseLineOpts = parseFile <|> parseRepl

parseOptions :: Parser Options
parseOptions = Options <$> parseLineOpts <*> pure emptyFlags

runFile :: CompilerState -> FilePath -> IO ()
runFile cs fname = do
  mtext <- getFileContents fname
  let cs' = cs { _fname = Just fname, _src = mtext }

  (res, _) <- runCompilerM compileFile cs'
  case res of
    Left err -> do
      T.putStrLn $ ppg err
      exitFailure
    Right _  -> return ()

krillEntry :: Options -> IO ()
krillEntry opts = do
  (_, cs') <- runCompilerM loadArgs cs
  (cm , cs'') <- runCompilerM loadStdlib cs'
  case cm of
    Right _  -> entry cs''
    Left err -> T.putStrLn $ ppg err
  where
    cs = emptyCS { _flags = flags opts }
    entry cs' = case lineOpt opts of
      UseReplLineOpts ->
        repl cs'
      RunFileLineOpts (fname:_) ->
        runFile cs' fname

cliIFace :: IO ()
cliIFace = customExecParser p opts >>= krillEntry
  where
    p  = prefs showHelpOnEmpty
    opts = info (helper <*> parseOptions)
      (  fullDesc
      <> header "The Krill Language"
      <> progDesc "Repl and iterpreter for the Krill programming language.")
