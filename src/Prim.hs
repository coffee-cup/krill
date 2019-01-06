module Prim where

import           Control.Monad.Except
import qualified Data.Text.Lazy       as T

import           Value

type Prim = [(T.Text, Value)]
type Unary = Value -> Eval Value
type Binary = Value -> Value -> Eval Value

mkF :: ([Value] -> Eval Value) -> Value
mkF = Fun . IFunc

primEnv :: Prim
primEnv = [ ("+", mkF $ binop $ numOp (+))
          ]

unop :: Unary -> [Value] -> Eval Value
unop op [x] = op x
unop _ args = throwError $ NumArgs 1 args

binop :: Binary -> [Value] -> Eval Value
binop op [x, y] = op x y
binop _ args    = throwError $ NumArgs 2 args

numOp :: (Double -> Double -> Double) -> Value -> Value -> Eval Value
numOp op (Number x) (Number y) = return $ Number $ op x  y
numOp _ Nil        (Number y)  = return $ Number y
numOp _ (Number x) Nil         = return $ Number x
numOp _ x          (Number _)  = throwError $ TypeMismatch "numeric op " x
numOp _ (Number _)  y          = throwError $ TypeMismatch "numeric op " y
numOp _ x _                    = throwError $ TypeMismatch "numeric op " x
