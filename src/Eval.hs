{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Eval where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.Reader
import           Data.Map             as Map
import           Data.Monoid
import           Data.Text.Lazy       as T
import           Data.Text.Lazy.IO    as T

import           Prim
import           Syntax
import           Value

basicEnv :: EnvCtx
basicEnv = Map.fromList primEnv

getOperator :: Name -> Eval Value
getOperator n = do
  env <- ask
  case Map.lookup n env of
    Just x  -> return x
    Nothing -> throwError $ OperatorNotFound n

evalLit :: Literal -> Eval Value
evalLit (LitNumber x) = return $ Number x
evalLit (LitString x) = return $ String x
evalLit (LitChar x)   = return $ Char x
evalLit (LitBool x)   = return $ Bool x
evalLit (LitAtom x)   = return $ Atom x

evalExpr :: Expr -> Eval Value
evalExpr (ELit l) = evalLit l
evalExpr (EApp e1 e2) = do
  env <- ask
  funVar <- evalExpr e1
  arg <- evalExpr e2
  case funVar of
    (Fun (IFunc fn))             -> fn [arg]
    (Lambda (IFunc fn) boundenv) -> do
      let f = fn [arg]
      r <- local (const (boundenv <> env)) f
      return r
    _ -> throwError $ NotFunction funVar
evalExpr (EBinOp n e1 e2) = do
  env <- ask
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  funVar <- getOperator n
  case funVar of
    (Fun (IFunc fn))             -> fn [v1, v2]
    (Lambda (IFunc fn) boundenv) -> do
      let f = fn [v1, v2]
      local (const (boundenv <> env)) f
    _ -> throwError $ NotFunction funVar

evalExpr _ = throwError $ Default "eval expr fall through"

evalStmt :: Stmt -> Eval Value
evalStmt (SExpr e) = evalExpr e
evalStmt _         = throwError $ Default "eval stmt fall through"
