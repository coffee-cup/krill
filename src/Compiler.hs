{-# LANGUAGE TemplateHaskell #-}

module Compiler where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.FileEmbed
import           Data.String
import           Data.Text.Lazy       as T
import           Data.Text.Lazy.IO    as T
import           System.Directory
import           System.Environment

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
  msrc <- gets _src
  Just fname <- gets _fname
  case msrc of
    Just _  -> go
    Nothing -> throwError $ CompilerError.FileNotFound fname
  where
    go = do
      mod <- Compiler.parseModule
      es <- gets _evalS
      (val, es') <- inIO $ runEval (evalModule mod) es
      case val of
        Right _ -> do
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

stdlibContents :: IsString a => a
stdlibContents = $(embedStringFile "stdlib/core.kr")

loadStdlib :: CompilerM ()
loadStdlib = do
  case Parser.Parser.parseModule (T.pack "stdlib.kr") stdlibContents of
    Right mod -> do
      es <- gets _evalS
      (val, es') <- inIO $ runEval (evalModule mod) es
      case val of
        Right _ -> modify (\st -> st { _evalS = es' })
        Left _  -> throwError $ StdlibError "Unable to eval"
    Left s -> throwError $ StdlibError s

loadArgs :: CompilerM ()
loadArgs = do
  args <- inIO $ getArgs
  es <- gets _evalS
  let env = _env es
  let valueArgs = fmap (Eval.Value.String . T.pack) args
  let env' = setValue "args" (List valueArgs) env
  let es' = es { _env = env' }
  modify (\st -> st { _evalS = es' })
  return ()

