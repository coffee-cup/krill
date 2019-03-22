{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Eval.BuiltIn where

import           Control.Monad.Except
import           Data.Text.Lazy.IO    as T

import           Eval.Value
import           Parser.Syntax
import           Pretty

builtIns :: [(Name, Value)]
builtIns =
  [ ("print", Lambda (IFunc Eval.BuiltIn.print) [])
  ]

print :: Value -> Eval Value
print v = do
  liftIO $ T.putStrLn $ ppg v
  return Unit
