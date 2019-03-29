{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Eval.Eval where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.List            as L
import qualified Data.Map             as Map
import           Data.Text.Lazy       as T

import           Eval.Env
import           Eval.Value
import           Parser.Syntax

type Binary = Expr -> Expr -> Eval Value
type Unary = Expr -> Eval Value

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

binOperators :: Map.Map T.Text Binary
binOperators = Map.fromList
  [ ("+", numBinOp (+))
  , ("*", numBinOp (*))
  , ("/", numBinOp (/))
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
evalLit LitUnit       = return $ Unit

evalBlock :: Block -> Eval Value
evalBlock (Block []) = return Unit
evalBlock (Block stmts) = do
  vals <- mapM evalStmt stmts
  return $ L.last vals

buildLamFunc :: Block -> [Name] -> Eval Value
buildLamFunc b [] = do
  val <- evalBlock b
  return val
buildLamFunc b (x:xs) = do
  env <- gets _env
  return $ Lambda (IFunc curriedFunc) env
  where
    curriedFunc :: Value -> Eval Value
    curriedFunc val = do
      env' <- gets _env
      let newScope = enterScope env'
      useEnv (setValue x val newScope)
      buildLamFunc b xs

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
evalExpr (EVar l n) = do
  env <- gets _env
  case getValue n env of
    Nothing  -> throwError $ UnboundVar l n
    Just val -> return val
evalExpr (EParens _ e) = evalExpr e
evalExpr (ELam _ params b) =
  buildLamFunc b params
evalExpr (EIf _ eCond eThen eElse) = do
  cond <- evalExpr eCond
  case cond of
    Bool True -> evalBlock eThen
    Bool False -> evalBlock eElse
    _ -> throwError $ TypeMismatch (loc eCond) "Condition to be boolean" cond
evalExpr (EFor _ n eList b) = do
  list <- evalExpr eList
  env <- gets _env
  case list of
    List xs -> do
      forM_ xs (\v -> do
                   modify (\st -> st { _env = setValue n v env })
                   evalBlock b)
      modify (\st -> st { _env = env })
      return Unit
    v -> throwError $ NotList (loc eList) v
evalExpr (EAss l n e) = do
  env <- gets _env
  let isBound = inInnerScope n env
  if isBound
  then throwError $ VariableAlreadyBound l n
  else do
    val <- evalExpr e
    case val of
      Lambda func lEnv -> do
        -- modify environment to allow recursive functions
        let newVal = Lambda func (setValue n newVal lEnv)
        modify (\st -> st { _env = setValue n newVal env })
        return val
      _ -> do
        modify (\st -> st { _env = setValue n val env })
        return val
evalExpr (EList _ xs) = do
  vs <- mapM evalExpr xs
  return $ List vs
evalExpr (EListAcc l eName eIdx) = do
  mList <- evalExpr (EVar l eName)
  idx <- evalExpr eIdx
  case idx of
    Number n ->
      if isInt n then
        case mList of
            List xs ->
              if (L.length xs) <= (round n)
              then throwError $ IndexOutOfRange l (round n)
              else return (xs !! (round n))
            _ ->
              throwError $ VariableNotAList l eName
      else throwError $ IndexNotAnInteger l idx
    _ ->
      throwError $ IndexNotAnInteger l mList
evalExpr (EApp l e1 e2) = do
  fun <- evalExpr e1
  arg <- evalExpr e2
  oldEnv <- gets _env
  case fun of
    Lambda (IFunc fn) env -> do
      modify (\st -> st { _env = env })
      val <- fn arg
      modify (\st -> st { _env = oldEnv })
      return val
    BuiltIn (BFunc fn) -> do
      val <- fn (loc e2) arg
      return val
    _ -> throwError $ NotFunction l arg

evalStmt :: Stmt -> Eval Value
evalStmt (SExpr _ e) = evalExpr e

evalModule :: Module -> Eval ()
evalModule (Module stmts) = mapM_ evalStmt stmts

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
  return . Bool $ go v1 v2
  where
    go :: Value -> Value -> Bool
    go (Atom x) (Atom y)     = x == y
    go (Number x) (Number y) = x == y
    go (String x) (String y) = x == y
    go (Bool x) (Bool y)     = x == y
    go (List xs) (List ys)   =
      (L.length xs) == (L.length ys) && L.all goZipped (L.zip xs ys)
    go Unit Unit             = True
    go _ _                   = False
    goZipped :: (Value, Value) -> Bool
    goZipped (x, y) = go x y

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

useEnv :: Env -> Eval ()
useEnv env = modify (\st -> st { _env = env } )

