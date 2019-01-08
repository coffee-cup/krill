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

runFunction :: Value -> [Value] -> Eval Value
runFunction funVar args = do
  env <- ask
  case funVar of
    (Fun (IFunc fn))             -> fn args
    (Lambda (IFunc fn) boundenv) -> do
      let f = fn args
      local (const (boundenv <> env)) f
    _ -> throwError $ NotFunction funVar

evalLit :: Literal -> Eval Value
evalLit (LitNumber x) = return $ Number x
evalLit (LitString x) = return $ String x
evalLit (LitChar x)   = return $ Char x
evalLit (LitBool x)   = return $ Bool x
evalLit (LitAtom x)   = return $ Atom x

evalExpr :: Expr -> Eval Value
evalExpr (ELit _ l) = evalLit l
evalExpr (EApp _ e1 e2) = do
  funVar <- evalExpr e1
  arg <- evalExpr e2
  runFunction funVar [arg]
evalExpr (EBinOp _ n e1 e2) = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  funVar <- getOperator n
  runFunction funVar [v1, v2]
evalExpr (EUnOp _ n e) = do
  v <- evalExpr e
  funVar <- getOperator n
  runFunction funVar [v]
evalExpr (EParens _ e) = evalExpr e

evalExpr _ = throwError $ Default "eval expr fall through"

evalStmt :: Stmt -> Eval Value
evalStmt (SExpr _ e) = evalExpr e
evalStmt _           = throwError $ Default "eval stmt fall through"
