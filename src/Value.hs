{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Value where

import           Control.Monad.Except
import           Control.Monad.Fail   hiding (fail)
import           Control.Monad.State
import qualified Data.Map             as Map
import qualified Data.Text.Lazy       as T

import           Syntax

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
  | List [Value]
  | Unit
  deriving (Eq)

newtype IFunc = IFunc (Value -> Eval Value)

instance Eq IFunc where
  (==) _ _ = False

data EvalError
  = TypeMismatch Loc T.Text Value
  | UnboundVar Loc T.Text
  | NumArgs Loc Integer Integer
  | NotFunction Loc Value
  | OperatorNotFound Loc Name
  | VariableAlreadyBound Loc T.Text
  | VariableNotAList Loc T.Text
  | IndexNotAnInteger Loc Value
  | IndexOutOfRange Loc Integer
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

basicState :: EvalState
basicState = EvalState [Map.empty]

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

runEval :: Eval a -> EvalState -> IO (Either EvalError a, EvalState)
runEval = runStateT . runExceptT . unEval
