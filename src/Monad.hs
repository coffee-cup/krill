{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Monad where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.State
import qualified Data.Text.Lazy       as L

import           CompilerError
import qualified Flags
import           Syntax

-- Compiler Monad

type CompilerMonad =
  ExceptT CompilerError (StateT CompilerState IO)

newtype CompilerM a = Compiler { runCompiler :: CompilerMonad a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadIO
    , MonadFail
    , MonadState CompilerState
    , MonadError CompilerError
    )

-- Compiler State

data CompilerState = CompilerState
  { _fname :: Maybe FilePath
  , _src   :: Maybe L.Text
  , _ast   :: Maybe Module
  , _flags :: Flags.Flags
  } deriving (Show)

emptyCS :: CompilerState
emptyCS = CompilerState
  { _fname = Nothing
  , _src   = Nothing
  , _ast   = Nothing
  , _flags = Flags.emptyFlags
  }

-- Run the compiler pipeline
runCompilerM
  :: CompilerM a -> CompilerState -> IO (Either CompilerError a, CompilerState)
runCompilerM = runStateT . runExceptT . runCompiler

-- Life IO action into the Compiler IO layer
inIO :: IO a -> CompilerM a
inIO = Compiler . liftIO

-- Conditional execute mondic action if a flag is set
ifSet :: (Flags.Flags -> Bool) -> CompilerM a -> CompilerM ()
ifSet f m = do
  flags <- gets _flags
  when (f flags) (void m)
