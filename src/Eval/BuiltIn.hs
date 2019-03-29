{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Eval.BuiltIn where

import           Control.Monad
import           Control.Monad.Except
import           Data.Text.Lazy.IO    as T

import           Eval.Value
import           Parser.Syntax
import           Pretty

builtIns :: [(Name, Value)]
builtIns =
  [ ("print", mkB Eval.BuiltIn.print)
  , ("length", mkB Eval.BuiltIn.length)
  , ("map", mkB Eval.BuiltIn.map)
  ]

mkB :: (Loc -> Value -> Eval Value) -> Value
mkB fn = BuiltIn $ BFunc fn

print :: Loc -> Value -> Eval Value
print _ v = do
  liftIO $ T.putStrLn $ ppg v
  return Unit

length :: Loc -> Value -> Eval Value
length l v = case v of
  List xs ->
    return $ Number $ fromIntegral $ Prelude.length xs
  _ -> throwError $ TypeMismatch l "argument to be list" v

map :: Loc -> Value -> Eval Value
map l1 arg1 =
  return $ BuiltIn $ BFunc mapFunc
  where
    mapFunc :: Loc -> Value -> Eval Value
    mapFunc l2 arg2 =
      case arg1 of
        Lambda (IFunc fn) _ ->
          case arg2 of
            List xs -> do
              ys <- mapM fn xs
              return $ List ys
