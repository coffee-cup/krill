{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval.Value where

import           Control.Monad.Except
import           Control.Monad.Fail   hiding (fail)
import           Control.Monad.State
import qualified Data.Map             as Map
import qualified Data.Text.Lazy       as T

import           Parser.Syntax

type Env = [Map.Map Name Value]

data EvalState = EvalState
  { _env :: Env
  } deriving (Eq)

type EvalMonad =
  ExceptT EvalError (StateT EvalState IO)

newtype Eval a = Eval { unEval :: EvalMonad a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadIO
    , MonadFail
    , MonadState EvalState
    , MonadError EvalError
    )

data Value
  = Atom T.Text
  | Number Double
  | String T.Text
  | Char Char
  | Bool Bool
  | Lambda IFunc Env
  | BuiltIn BFunc
  | List [Value]
  | Unit
  deriving (Eq)

newtype IFunc = IFunc (Value -> Eval Value)
newtype BFunc = BFunc (Loc -> Value -> Eval Value)

instance Eq IFunc where
  (==) _ _ = False

instance Eq BFunc where
  (==) _ _ = False

data EvalError
  = TypeMismatch Loc T.Text Value
  | UnboundVar Loc T.Text
  | NumArgs Loc Integer Integer
  | NotFunction Loc Value
  | NotList Loc Value
  | OperatorNotFound Loc Name
  | VariableAlreadyBound Loc T.Text
  | VariableNotAList Loc T.Text
  | IndexNotAnInteger Loc Value
  | IndexOutOfRange Loc Integer
  | NoParse Loc T.Text Value
  | FileNotFound Loc T.Text
  | ThrowError Loc T.Text
  | Default Loc T.Text
  deriving (Eq)

instance Location EvalError where
  loc e = case e of
    TypeMismatch l _ _   -> l
    UnboundVar l _       -> l
    NumArgs l _ _        -> l
    NotFunction l _      -> l
    OperatorNotFound l _ -> l
    Default l _          -> l

runEval :: Eval a -> EvalState -> IO (Either EvalError a, EvalState)
runEval = runStateT . runExceptT . unEval

enterScope :: Env -> Env
enterScope xs = Map.empty : xs

endScope :: Env -> Env
endScope (_:xs) = xs
endScope []     = []

inInnerScope :: Name -> Env -> Bool
inInnerScope n (x:_) = Map.member n x
inInnerScope _ []    = False

getValue :: Name -> Env -> Maybe Value
getValue n (x:xs) =
  case Map.lookup n x of
    Just r  -> Just r
    Nothing -> getValue n xs
getValue _ [] = Nothing

setValue :: Name -> Value -> Env -> Env
setValue n v (x:xs) = Map.insert n v x : xs
setValue _ _ []     = error "inserting into empty scope"

mergeEnvs :: Env -> Env -> Env
mergeEnvs (x:xs) (y:ys) = Map.union x y : mergeEnvs xs ys
mergeEnvs (x:xs) []     = x : mergeEnvs xs []
mergeEnvs [] (y:ys)     = y : mergeEnvs [] ys
mergeEnvs [] []         = []

evalInEnv :: Eval a -> Env -> Eval a
evalInEnv eval env = do
  oldEnv <- gets _env
  let newEnv = mergeEnvs env oldEnv
  modify (\st -> st { _env = newEnv })
  res <- eval
  modify (\st -> st { _env = oldEnv })
  return res
