{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Eval.BuiltIn where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Foldable
import           Data.Text.Lazy       as T
import           Data.Text.Lazy.IO    as T
import           Text.Read            hiding (Number, String)

import           Eval.Value
import           Parser.Syntax
import           Pretty

builtIns :: [(Name, Value)]
builtIns =
  [ ("print", mkB Eval.BuiltIn.print)
  , ("length", mkB Eval.BuiltIn.length)
  , ("map", mkB Eval.BuiltIn.map)
  , ("foldl", mkB Eval.BuiltIn.foldl)
  , ("foldr", mkB Eval.BuiltIn.foldr)
  , ("toNumber", mkB Eval.BuiltIn.toNumber)
  , ("toString", mkB Eval.BuiltIn.toString)
  ]

mkB :: (Loc -> Value -> Eval Value) -> Value
mkB fn = BuiltIn $ BFunc fn

checkListArg :: Loc -> Value -> Eval ()
checkListArg _ (List _) = return ()
checkListArg l arg      = throwError $ TypeMismatch l "list" arg

checkFnArg :: Loc -> Value -> Eval ()
checkFnArg _ (Lambda _ _) = return ()
checkFnArg _ (BuiltIn _)  = return ()
checkFnArg l arg          = throwError $ TypeMismatch l "function" arg

checkStringArg :: Loc -> Value -> Eval ()
checkStringArg _ (String _) = return ()
checkStringArg l arg        =  throwError $ TypeMismatch l "string" arg

print :: Loc -> Value -> Eval Value
print _ v = do
  liftIO $ T.putStrLn $ ppg v
  return Unit

length :: Loc -> Value -> Eval Value
length _ (List xs)  = return $ Number $ fromIntegral $ Prelude.length xs
length _ (String s) = return $ Number $ fromIntegral $ T.length s
length  l  v        = throwError $ TypeMismatch l "list" v

map :: Loc -> Value -> Eval Value
map l1 arg1 = do
  checkFnArg l1 arg1
  return $ BuiltIn $ BFunc (\l2 arg2 -> do
                               checkListArg l2 arg2
                               evalMap l2 arg1 arg2)
  where
    evalMap :: Loc -> Value -> Value -> Eval Value
    evalMap _ (Lambda (IFunc fn) env) (List xs) = do
        oldEnv <- gets _env
        modify (\st -> st { _env = env })
        ys <- mapM fn xs
        modify (\st -> st { _env = oldEnv })
        return $ List ys
    evalMap l (BuiltIn (BFunc fn)) (List xs) = do
        ys <- mapM (fn l) xs
        return $ List ys
    evalMap l _ _ = throwError $ Default l "evalMap fall through"

foldBuiltIn ::
  ((Value -> Value -> Eval Value) -> Value -> [Value] -> Eval Value)
  -> Loc
  -> Value
  -> Eval Value
foldBuiltIn foldFnM l1 argFn = do
  checkFnArg l1 argFn
  return $ BuiltIn $ BFunc (\l2 argInit ->
                              return
                              $ BuiltIn
                              $ BFunc (\l3 argList -> do
                                          checkListArg l3 argList
                                          evalFold argFn argInit argList))
  where
    foldFn :: (Value -> Eval Value) -> Value -> Value -> Eval Value
    foldFn fn1 acc curr = do
      accApplied <- fn1 acc
      case accApplied of
        Lambda (IFunc fn2) env2 -> do
          oldEnv <- gets _env
          modify (\st -> st { _env = env2 })
          next <- fn2 curr
          modify (\st -> st { _env = oldEnv })
          return next
        BuiltIn (BFunc fn2) -> fn2 l1 curr
        _                       -> throwError $ NumArgs l1 2 1

    evalFold :: Value -> Value -> Value -> Eval Value
    evalFold lam@(Lambda (IFunc fn) env) init (List xs) = do
      oldEnv <- gets _env
      modify (\st -> st { _env = env })
      res <- foldFnM (foldFn fn) init xs
      modify (\st -> st { _env = oldEnv })
      return res
    evalFold bi@(BuiltIn (BFunc fn)) init (List xs) = do
      res <- foldlM (foldFn (fn l1)) init xs
      return res

foldl :: Loc -> Value -> Eval Value
foldl = foldBuiltIn foldlM

foldr :: Loc -> Value -> Eval Value
foldr = foldBuiltIn foldrM

toNumber :: Loc -> Value -> Eval Value
toNumber l v@(String s) = case (readMaybe $ T.unpack s :: Maybe Double) of
  Just n  -> return $ Number n
  Nothing -> throwError $ NoParse l "number" v
toNumber l v = throwError $ NoParse l "number" v

toString :: Loc -> Value -> Eval Value
toString _ v = return $ String $ ppg v

