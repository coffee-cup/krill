module Compiler where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Text.Lazy       as T
import           Data.Text.Lazy.IO    as T
import           System.Directory

import           CompilerError
import           Eval.Eval
import           Eval.Value
import           Flags
import           Monad
import           Parser.Parser
import           Parser.Syntax
import           Pretty

compileFile :: CompilerM ()
compileFile = do
  mod <- Compiler.parseModule
  es <- gets _evalS
  (val, es') <- inIO $ runEval (evalModule mod) es
  case val of
    Right val' -> do
      modify (\st -> st { _evalS = es' })
    Left e     -> throwError $ EvaluationError e

compileLine :: CompilerM ()
compileLine = do
  Just text <- gets _src
  stmt <- parseText text
  es <- gets _evalS
  (val, es') <- inIO $ runEval (evalStmt stmt) es
  case val of
    Right val' -> do
      modify (\st -> st { _evalS = es' })
      inIO $ T.putStrLn $ ppg val'
    Left e     -> throwError $ EvaluationError e

parseModule :: CompilerM Module
parseModule = do
  Just fname <- gets _fname
  Just src <- gets _src
  case Parser.Parser.parseModule (T.pack fname) src of
    Right mod -> do
      ifSet dumpAst (dumpValues "Ast" mod)
      return mod
    Left s -> throwError $ ParseError s

parseText :: T.Text -> CompilerM Stmt
parseText input = do
  let ast = parseStmt "<repl>" input
  ifSet dumpAst (dumpValues "Ast" ast)
  case ast of
    Right ast' -> return ast'
    Left s     -> throwError $ ParseError s

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

stdlibCore :: FilePath
stdlibCore = "stdlib/core.kr"

loadStdlib :: CompilerM ()
loadStdlib = do
  mCore <- inIO $ getFileContents stdlibCore
  case mCore of
    Just core -> case Parser.Parser.parseModule (T.pack stdlibCore) core of
      Right mod -> do
        es <- gets _evalS
        (val, es') <- inIO $ runEval (evalModule mod) es
        case val of
          Right _  -> modify (\st -> st { _evalS = es' })
          Left err -> throwError $ StdlibError "Unable to eval"
      Left s -> throwError $ StdlibError "Unable to parse"
    Nothing -> throwError $ StdlibNotFound stdlibCore

