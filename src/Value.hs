{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Value where

import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.Reader
import qualified Data.Map             as Map
import qualified Data.Text.Lazy       as T

import           Syntax

type EnvCtx = Map.Map T.Text Value

type EvalMonad =
  ExceptT EvalError (ReaderT EnvCtx IO)

newtype Eval a = Eval { unEval :: EvalMonad a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadIO
    , MonadFail
    , MonadReader EnvCtx
    , MonadError EvalError
    )

data Value
  = Atom T.Text
  | Number Double
  | String T.Text
  | Char Char
  | Bool Bool
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  deriving (Eq)

data IFunc = IFunc { fn :: [Value] -> Eval Value }

instance Eq IFunc where
  (==) _ _ = False

data EvalError
  = TypeMismatch Loc T.Text Value
  | UnboundVar Loc T.Text
  | NumArgs Loc Integer Integer
  | NotFunction Loc Value
  | OperatorNotFound Loc Name
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

runEval :: Eval a -> EnvCtx -> IO (Either EvalError a)
runEval = runReaderT . runExceptT . unEval
