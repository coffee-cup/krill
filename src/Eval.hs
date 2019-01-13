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

import           Syntax
import           Value

type Binary = Expr -> Expr -> Eval Value
type Unary = Expr -> Eval Value

binOperators :: Map.Map T.Text Binary
binOperators = Map.fromList
  [ ("+", numBinOp (+))
  , ("*", numBinOp (*))
  , ("/",  numBinOp (/))
  , ("-", numBinOp (-))
  , ("<", numBinCmp (<))
  , (">", numBinCmp (>))
  , ("<=", numBinCmp (<=))
  , (">=", numBinCmp (>=))
  , ("&&", eqBinOp (&&))
  , ("||", eqBinOp (||))
  , ("==", eqCmd)
  ]

unOperators :: Map.Map T.Text Unary
unOperators = Map.fromList
  [ ("-", numUnOp negate)
  , ("!", eqUnCmp not)
  ]

evalLit :: Literal -> Eval Value
evalLit (LitNumber x) = return $ Number x
evalLit (LitString x) = return $ String x
evalLit (LitChar x)   = return $ Char x
evalLit (LitBool x)   = return $ Bool x
evalLit (LitAtom x)   = return $ Atom x

evalExpr :: Expr -> Eval Value
evalExpr (ELit _ l) = evalLit l
evalExpr (EUnOp l n e1) =
  case Map.lookup n unOperators of
    Nothing -> throwError $ OperatorNotFound l n
    Just fn -> fn e1
evalExpr (EBinOp l n e1 e2) =
  case Map.lookup n binOperators of
    Nothing -> throwError $ OperatorNotFound l n
    Just fn -> fn e1 e2
evalExpr (EParens _ e) = evalExpr e
evalExpr e = throwError $ Default (loc e) "eval expr fall through"

evalStmt :: Stmt -> Eval Value
evalStmt (SExpr _ e) = evalExpr e
evalStmt ex          = throwError $ Default (loc ex) "eval stmt fall through"

numBinOp :: (Double -> Double -> Double) -> Expr -> Expr -> Eval Value
numBinOp op e1 e2 = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  go op v1 v2
  where
    go op (Number x) (Number y) = return $ Number $ op x y
    go _ v (Number _) =
      throwError $ TypeMismatch (loc e1) "LHS to be number" v
    go _ _ v =
      throwError $ TypeMismatch (loc e2) "RHS to be number" v

numBinCmp :: (Double -> Double -> Bool) -> Expr -> Expr -> Eval Value
numBinCmp op e1 e2 = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  go op v1 v2
  where
    go op (Number x) (Number y) = return $ Bool $ op x y
    go _ v (Number _) =
      throwError $ TypeMismatch (loc e1) "LHS to be boolean" v
    go _ _ v =
      throwError $ TypeMismatch (loc e2) "RHS to be boolean" v

eqBinOp :: (Bool -> Bool -> Bool) -> Expr -> Expr -> Eval Value
eqBinOp op e1 e2 = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  go op v1 v2
  where
    go op (Bool x) (Bool y) = return $ Bool $ op x y
    go _ v (Bool _) =
      throwError $ TypeMismatch (loc e1) "LHS to be boolean" v
    go _ _ v =
      throwError $ TypeMismatch (loc e2) "RHS to be boolean" v

eqCmd :: Expr -> Expr -> Eval Value
eqCmd e1 e2 = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  go v1 v2
  where
    go (Atom x) (Atom y)     = return . Bool $ x == y
    go (Number x) (Number y) = return . Bool $ x == y
    go (String x) (String y) = return . Bool $ x == y
    go (Bool x) (Bool y)     = return . Bool $ x == y
    go Nil Nil               = return $ Bool True
    go _ _                   = return $ Bool False

numUnOp :: (Double -> Double) -> Expr -> Eval Value
numUnOp op e1 = do
  v1 <- evalExpr e1
  case v1 of
    (Number x) -> return $ Number $ op x
    v          -> throwError $ TypeMismatch (loc e1) "Operand to be number" v

eqUnCmp :: (Bool -> Bool) -> Expr -> Eval Value
eqUnCmp op e1 = do
  v1 <- evalExpr e1
  case v1 of
    (Bool x) -> return $ Bool $ op x
    v        -> throwError $ TypeMismatch (loc e1) "Operand to be boolean" v
