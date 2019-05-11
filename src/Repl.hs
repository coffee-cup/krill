{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Repl
  ( repl
  ) where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.List                  (isPrefixOf)
import qualified Data.Text.Lazy             as T
import qualified Data.Text.Lazy.IO          as T
import           System.Console.Repline
import           System.Exit

import           Compiler
import           Eval.Value
import           Flags
import           Monad
import           Pretty

-- Types

data IState = IState
  { _compilerState :: CompilerState
  }

initState :: CompilerState -> IState
initState = IState

type Repl a = HaskelineT (StateT IState IO) a

hoistErr :: Pretty e => Either e a -> Repl ()
hoistErr (Right _) = return ()
hoistErr (Left err) =
  liftIO $ T.putStrLn $ ppg err

updateCompilerState :: CompilerState -> Repl ()
updateCompilerState cs = modify (\st -> st { _compilerState = cs })

-- Execution

exec :: CompilerM () -> Repl ()
exec compM = do
  cs <- gets _compilerState
  (cm , cs') <- liftIO $ runCompilerM compM cs
  hoistErr cm

  updateCompilerState cs'
  return ()

execLine :: T.Text -> Repl ()
execLine source = do
  cs <- gets _compilerState
  updateCompilerState (cs { _src = Just source })
  exec compileLine

execFile :: FilePath -> Repl ()
execFile fname = do
  mtext <- liftIO $ getFileContents fname
  cs <- gets _compilerState
  updateCompilerState cs { _fname = Just fname, _src = mtext }
  exec (void compileFile)

cmd :: String -> Repl ()
cmd source = execLine (T.pack source)

-- Commands

showMsg :: String -> Repl ()
showMsg s = liftIO $ T.putStrLn $ T.pack s

-- Eventually change colour to red
showError :: String -> Repl ()
showError s = liftIO $ T.putStrLn $ T.pack s

changeFlag :: [String] -> String -> Bool -> Repl ()
changeFlag [flag] _ change = do
  cs <- gets _compilerState
  let flags = _flags cs
  case setFlag flag change flags of
    Just flags' -> do
      let cs' = cs { _flags = flags' }
      modify (\st -> st { _compilerState = cs' })
    Nothing -> showError $ "Flag " ++ flag ++ " is invalid"
changeFlag _ name _ = showError $ name ++ " command requires flag name as argument"

load :: [String] -> Repl ()
load []         = showError "load requires a filename"
load (fname:[]) = execFile fname
load _          = showError "load requires a single filename"

clear :: [String] -> Repl ()
clear [] = showError "clear requires a parameter name"
clear xs = mapM_ go xs
  where
    go n = do
      cs <- gets _compilerState
      let es = _evalS cs
      let env = _env es
      let newEnv = clearValue (T.pack n) env
      let cs' = cs { _evalS = es { _env = newEnv } }
      updateCompilerState cs'

help :: a -> Repl ()
help _ = showMsg "Commands available \n\
\  .load \t load source file into repl \n\
\  .clear [name] \t clear name from env. \n\
\  .help \t show this message \n\
\  .quit \t quit the repl "

quit :: a -> Repl ()
quit _ = liftIO exitSuccess

-- Interactive Shell

-- Prefix tab completer
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher =
  [ (".load", fileCompleter)
  ]

-- Default tab completer
comp :: (Monad m, MonadState IState m) => WordCompleter m
comp n = do
  let cmds =
        [ ".load"
        , ".quit"
        , ".help"
        , ".clear"
        ]
  return $ filter (isPrefixOf n) cmds

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter comp) defaultMatcher

options :: [(String, [String] -> Repl ())]
options =
  [ ("load", load)
  , ("help", help)
  , ("quit", quit)
  , ("clear", clear)
  ]

-- Entry Point

ini :: Repl ()
ini = liftIO $ T.putStrLn $ T.pack banner

repl :: CompilerState -> IO ()
repl cs = flip evalStateT (initState cs)
  $ evalRepl (pure "λ ") cmd options (Just '.') completer ini
