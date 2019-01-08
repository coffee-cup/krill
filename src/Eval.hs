{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Eval where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.Reader
import qualified Data.Map             as Map
import           Data.Monoid
import           Data.Text.Lazy       as T
import           Data.Text.Lazy.IO    as T

import           Prim
import           Syntax
import           Value

type Binary = Expr -> Expr -> Eval Value

basicEnv :: EnvCtx
basicEnv = Map.fromList primEnv

binOperators :: Map.Map T.Text Binary
binOperators = Map.fromList
  [ ("+", numOp (+))
  ]

numOp :: (Double -> Double -> Double) -> Expr -> Expr -> Eval Value
numOp op e1 e2 = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  go op v1 v2
  where
    go :: (Double -> Double -> Double) -> Value -> Value -> Eval Value
    go op (Number x) (Number y) = return $ Number $ op x y
    go _ v (Number y) =
      throwError $ TypeMismatch (loc e1) "LHS to be number" v
    go _ _ v =
      throwError $ TypeMismatch (loc e2) "RHS to be number" v

runFunction :: Loc -> Value -> [Value] -> Eval Value
runFunction l funVar args = do
  env <- ask
  case funVar of
    (Fun (IFunc fn))             -> fn args
    (Lambda (IFunc fn) boundenv) -> do
      let f = fn args
      local (const (boundenv <> env)) f
    _ -> throwError $ NotFunction l funVar

runOperator :: Loc -> Value -> [Value] -> Eval Value
runOperator l (Fun (IFunc fn)) args = do
  env <- ask
  res <- (Eval . liftIO) $ runEval (fn args) env
  case res of
    Left err -> throwError $ addLoc l err
    Right r  -> return r

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
  runFunction (loc e1) funVar [arg]
evalExpr ex@(EBinOp _ n e1 e2) =
  case Map.lookup n binOperators of
    Nothing -> throwError $ OperatorNotFound (loc ex) n
    Just fn -> fn e1 e2
evalExpr (EParens _ e) = evalExpr e

evalExpr e = throwError $ Default (loc e) "eval expr fall through"

evalStmt :: Stmt -> Eval Value
evalStmt (SExpr _ e) = evalExpr e
evalStmt ex          = throwError $ Default (loc ex) "eval stmt fall through"

