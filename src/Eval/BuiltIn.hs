{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Eval.BuiltIn where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
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
map l1 arg1 = do
  checkFnArg l1 arg1
  return $ BuiltIn $ BFunc (\l2 arg2 -> do
                               checkListArg l2 arg2
                               evalApp l2 arg1 arg2)
  where
    checkFnArg :: Loc -> Value -> Eval ()
    checkFnArg l arg =
      case arg of
        Lambda _ _ -> return ()
        BuiltIn _  -> return ()
        _ -> throwError $ TypeMismatch l "argument to be a function" arg
    checkListArg :: Loc -> Value -> Eval ()
    checkListArg l arg =
      case arg of
        List _ -> return ()
        _      -> throwError $ TypeMismatch l "argument to be a list" arg
    evalApp :: Loc -> Value -> Value -> Eval Value
    evalApp _ (Lambda (IFunc fn) env) (List xs) = do
      oldEnv <- gets _env
      modify (\st -> st { _env = env })
      ys <- mapM fn xs
      modify (\st -> st { _env = oldEnv })
      return $ List ys
    evalApp l (BuiltIn (BFunc fn)) (List xs) = do
      ys <- mapM (fn l) xs
      return $ List ys
