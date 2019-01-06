{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval where

import           Control.Monad.Reader
import qualified Data.Map             as Map
import qualified Data.Text.Lazy       as T

type EnvCtx = Map.Map T.Text Value

newtype Eval a = EVal { unEval :: ReaderT EnvCtx IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader EnvCtx
    , MonadIO
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
  = TypeMismatch T.Text Value
  | UnboundVar T.Text
  | Default Value
  deriving (Eq)
