module Prim (primEnv) where

import           Control.Monad.Except
import qualified Data.Text.Lazy       as T

import           Syntax
import           Value

type Prim = [(T.Text, Value)]
type Unary = Value -> Eval Value
type Binary = Value -> Value -> Eval Value

mkF :: ([Value] -> Eval Value) -> Value
mkF = Fun . IFunc

primEnv :: Prim
primEnv = [ ("+", mkF $ binop $ numOp (+))
          , ("*", mkF $ binop $ numOp (*))
          , ("/", mkF $ binop $ numOp (/))
          , ("-", mkF $ binunop (Number 0) $ numOp (-))
          , ("<", mkF $ binop $ numCmp (<))
          , (">", mkF $ binop $ numCmp (>))
          , ("<=", mkF $ binop $ numCmp (<=))
          , (">=", mkF $ binop $ numCmp (>=))
          , ("&&", mkF $ binop $ eqOp (&&))
          , ("||", mkF $ binop $ eqOp (||))
          , ("==", mkF $ binop eqCmd)
          ]

unop :: Unary -> [Value] -> Eval Value
unop op [x] = op x
unop _ args = throwError $ NumArgs NoLoc 1 (toInteger $ length args)

binop :: Binary -> [Value] -> Eval Value
binop op [x, y] = op x y
binop _ args    = throwError $ NumArgs NoLoc 2 (toInteger $ length args)

binunop :: Value -> Binary -> [Value] -> Eval Value
binunop d op [x]  = op d x
binunop _ op args = binop op args

numOp :: (Double -> Double -> Double) -> Value -> Value -> Eval Value
numOp op (Number x) (Number y) = return $ Number $ op x  y
numOp _ Nil        (Number y)  = return $ Number y
numOp _ (Number x) Nil         = return $ Number x
numOp _ x          (Number _)  = throwError $ TypeMismatch NoLoc "numeric op" x
numOp _ (Number _)  y          = throwError $ TypeMismatch NoLoc "numeric op" y
numOp _ x _                    = throwError $ TypeMismatch NoLoc "numeric op" x

numCmp :: (Double -> Double -> Bool) -> Value -> Value -> Eval Value
numCmp op (Number x) (Number y) = return . Bool $ op x y
numCmp _ x          (Number _)  = throwError $ TypeMismatch NoLoc "numeric op" x
numCmp _ (Number _)  y          = throwError $ TypeMismatch NoLoc "numeric op" y
numCmp _ x _                    = throwError $ TypeMismatch NoLoc "numeric op" x

eqOp :: (Bool -> Bool -> Bool) -> Value -> Value -> Eval Value
eqOp op (Bool x) (Bool y) = return $ Bool $ op x y
eqOp _  x       (Bool _)  = throwError $ TypeMismatch NoLoc "bool op" x
eqOp _ (Bool _)  y        = throwError $ TypeMismatch NoLoc "bool op" y
eqOp _ x _                = throwError $ TypeMismatch NoLoc "bool op" x

eqCmd :: Value -> Value -> Eval Value
eqCmd (Atom x) (Atom y)     = return . Bool $ x == y
eqCmd (Number x) (Number y) = return . Bool $ x == y
eqCmd (String x) (String y) = return . Bool $ x == y
eqCmd (Bool x) (Bool y)     = return . Bool $ x == y
eqCmd Nil Nil               = return $ Bool True
eqCmd _ _                   = return $ Bool False
