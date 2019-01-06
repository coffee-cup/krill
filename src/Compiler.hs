module Compiler where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Text.Lazy       as T
import           Data.Text.Lazy.IO    as T
import           System.Directory

import           CompilerError
import           Flags
import           Monad
import           Parser
import           Syntax

compileFile :: CompilerM Module
compileFile = do
  Just fname <- gets _fname
  Just src <- gets _src
  case parseModule (T.pack fname) src of
    Right mod -> do
      ifSet dumpAst (dumpValues "Frontend" mod)
      return mod
    Left s -> throwError $ ParseError s

dumpValues :: Show a => String -> a -> CompilerM ()
dumpValues header v = do
  inIO $ T.putStrLn $ T.pack $ "--- " ++ header
  inIO $ T.putStrLn $ T.pack $ show v
  inIO $ T.putStrLn ""

getFileContents :: FilePath -> IO (Maybe T.Text)
getFileContents fname = do
  exists <- doesFileExist fname
  if exists
    then do
      text <- T.readFile fname
      return $ Just text
    else return Nothing
