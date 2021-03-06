{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Eval.BuiltIn where

import           Control.Monad.Except
import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.Text.Lazy       as T
import           Data.Text.Lazy.IO    as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.LocalTime
import           System.Directory
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
  , ("readFile", mkB Eval.BuiltIn.readFile)
  , ("writeFile", mkB Eval.BuiltIn.writeFile)
  , ("appendFile", mkB Eval.BuiltIn.appendFile)
  , ("split", mkB Eval.BuiltIn.split)
  , ("intersperse", mkB Eval.BuiltIn.intersperse)
  , ("trim", mkB Eval.BuiltIn.trim)
  , ("isList", mkB Eval.BuiltIn.isList)
  , ("isNumber", mkB Eval.BuiltIn.isNumber)
  , ("isString", mkB Eval.BuiltIn.isString)
  , ("isBool", mkB Eval.BuiltIn.isBool)
  , ("isAtom", mkB Eval.BuiltIn.isAtom)
  , ("date", mkB Eval.BuiltIn.date)
  , ("time", mkB Eval.BuiltIn.time)
  , ("throwError", mkB Eval.BuiltIn.throw)
  , ("assert", mkB Eval.BuiltIn.assert)
  , ("root", mkB Eval.BuiltIn.root)
  , ("floor", mkB Eval.BuiltIn.floor)
  , ("ceil", mkB Eval.BuiltIn.ceil)
  , ("sin", mkB Eval.BuiltIn.sin)
  , ("cos", mkB Eval.BuiltIn.cos)
  , ("charToCode", mkB Eval.BuiltIn.charToCode)
  , ("codeToChar", mkB Eval.BuiltIn.codeToChar)
  ]

mkB :: (Loc -> Value -> Eval Value) -> Value
mkB fn = BuiltIn $ BFunc fn

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

chain :: (Loc -> Value -> Eval Value) -> Eval Value
chain = return . mkB

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

checkNumArg :: Loc -> Value -> Eval ()
checkNumArg _ (Number _) = return ()
checkNumArg l arg        = throwError $ TypeMismatch l "number" arg

print :: Loc -> Value -> Eval Value
print _ (String s) = do
  liftIO $ T.putStrLn s
  return Unit
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
  chain (\l2 arg2 -> do
            checkListArg l2 arg2
            evalMap l2 arg1 arg2)
  where
    evalMap :: Loc -> Value -> Value -> Eval Value
    evalMap _ (Lambda (IFunc fn) env) (List xs) = do
        ys <- evalInEnv (mapM fn xs) env
        return $ List ys
    evalMap l (BuiltIn (BFunc fn)) (List xs) = do
        ys <- mapM (fn l) xs
        return $ List ys
    evalMap _ _ _ = error "fall through map"

foldBuiltIn ::
  ((Value -> Value -> Eval Value) -> Value -> [Value] -> Eval Value)
  -> Loc
  -> Value
  -> Eval Value
foldBuiltIn foldFnM l1 argFn = do
  checkFnArg l1 argFn
  chain (\_ argInit ->
            chain (\l3 argList -> do
                      checkListArg l3 argList
                      evalFold argFn argInit argList))
  where
    foldFn :: (Value -> Eval Value) -> Value -> Value -> Eval Value
    foldFn fn1 acc curr = do
      accApplied <- fn1 acc
      case accApplied of
        Lambda (IFunc fn2) env2 -> evalInEnv (fn2 curr) env2
        BuiltIn (BFunc fn2)     -> fn2 l1 curr
        _                       -> throwError $ NumArgs l1 2 1

    evalFold :: Value -> Value -> Value -> Eval Value
    evalFold (Lambda (IFunc fn) env) init (List xs) =
      evalInEnv (foldFnM (foldFn fn) init xs) env
    evalFold (BuiltIn (BFunc fn)) init (List xs) = do
      res <- foldlM (foldFn (fn l1)) init xs
      return res
    evalFold _ _ _ = error "fall through fold"

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
toString _ v@(String _) = return v
toString _ v            = return $ String $ ppg v

readFile :: Loc -> Value -> Eval Value
readFile l (String fname) = do
  exists <- liftIO $ doesFileExist $ T.unpack fname
  if exists then do
    text <- liftIO $ T.readFile $ T.unpack fname
    return $ String text
  else throwError $ FileNotFound l fname
readFile l v = throwError $ TypeMismatch l "string" v

writeFile :: Loc -> Value -> Eval Value
writeFile l1 argFname = do
  checkStringArg l1 argFname
  chain (\l2 argText -> do
            checkStringArg l2 argText
            writeFileFn l1 argFname argText)
  where
    writeFileFn :: Loc -> Value -> Value -> Eval Value
    writeFileFn _ (String fname) (String text) = do
      liftIO $ T.writeFile (T.unpack fname) text
      return Unit
    writeFileFn _ _ _ = error "fall through writeFile"

appendFile :: Loc -> Value -> Eval Value
appendFile l1 argFname = do
  checkStringArg l1 argFname
  chain (\l2 argText -> do
            checkStringArg l2 argText
            appendFileFn l1 argFname argText)
  where
    appendFileFn :: Loc -> Value -> Value -> Eval Value
    appendFileFn _ (String fname) (String text) = do
      liftIO $ T.appendFile (T.unpack fname) text
      return Unit
    appendFileFn _ _ _ = error "fall through appendFile"

intersperse :: Loc -> Value -> Eval Value
intersperse l1 argDelim = do
  chain (\l2 argList -> do
            checkListArg l2 argList
            intersperseFn l1 argDelim argList)
  where
    intersperseFn :: Loc -> Value -> Value -> Eval Value
    intersperseFn _ delim (List list) =
      return $ List $ Data.List.intersperse delim list
    intersperseFn _ _ _ = error "fall through intersperse"

split :: Loc -> Value -> Eval Value
split l1 argDelim = do
  checkStringArg l1 argDelim
  chain (\l2 argText -> do
            checkStringArg l2 argText
            splitFn l1 argDelim argText)
  where
    splitFn :: Loc -> Value -> Value -> Eval Value
    splitFn _ (String delim) (String text) =
      if delim == ""
      then return $ List $ fmap Eval.Value.Char $ unpack text
      else
        let xs = T.splitOn delim text
        in return $ List (fmap String xs)
    splitFn _ _ _ = error "fall through split"

trim :: Loc -> Value -> Eval Value
trim _ (String s) = return $ String $ T.strip s
trim l v          = throwError $ TypeMismatch l "string" v

charToCode :: Loc -> Value -> Eval Value
charToCode _ (Eval.Value.Char c) = return $ Number $ fromIntegral $ ord c
charToCode l v                   = throwError $ TypeMismatch l "char" v

codeToChar :: Loc -> Value -> Eval Value
codeToChar l v@(Number n) =
  if Eval.BuiltIn.isInt n then
    if n >= 0 && n <= 127 then return $ Eval.Value.Char $ chr (Prelude.floor n)
    else throwError $ Default l "Input to codeToChar must be between 0 and 127"
  else throwError $ NotAnInteger l v
codeToChar l v          = throwError $ TypeMismatch l "number" v

isString :: Loc -> Value -> Eval Value
isString _ (String _) = return $ Bool True
isString _ _          = return $ Bool False

isNumber :: Loc -> Value -> Eval Value
isNumber _ (Number _) = return $ Bool True
isNumber _ _          = return $ Bool False

isList :: Loc -> Value -> Eval Value
isList _ (List _) = return $ Bool True
isList _ _        = return $ Bool False

isBool :: Loc -> Value -> Eval Value
isBool _ (Bool _) = return $ Bool True
isBool _ _        = return $ Bool False

isAtom :: Loc -> Value -> Eval Value
isAtom _ (Atom _) = return $ Bool True
isAtom _ _        = return $ Bool False

date :: Loc -> Value -> Eval Value
date _ _ = do
  now <- liftIO getCurrentTime
  timezone <- liftIO getCurrentTimeZone
  let zoneNow = utcToLocalTime timezone now
  let (year, month, day) = toGregorian $ localDay zoneNow
  return $ String $ T.pack $ (show year) ++ "-" ++ (show month) ++ "-" ++ (show day)

time :: Loc -> Value -> Eval Value
time _ _ = do
  now <- liftIO getCurrentTime
  timezone <- liftIO getCurrentTimeZone
  let (TimeOfDay hour minute _) = localTimeOfDay $ utcToLocalTime timezone now
  return $ String $ T.pack $ (show hour) ++ ":" ++ (show minute)

throw :: Loc -> Value -> Eval Value
throw l (String s) = throwError $ ThrowError l s
throw l v          = throwError $ ThrowError l (ppg v)

assert :: Loc -> Value -> Eval Value
assert l1 arg1 = do
  chain (\l2 arg2 -> assertFn l1 l2 arg1 arg2)
  where
    assertFn :: Loc -> Loc -> Value -> Value -> Eval Value
    assertFn l1 l2 arg1 arg2 = do
      (String s1) <- toString l1 arg1
      (String s2) <- toString l2 arg2
      if arg1 == arg2
      then return $ Unit
      else throwError $ ThrowError l1 $ s1 <> " does not equal " <> s2

nthRoot :: (Eq b, Floating b) => b -> b -> b
n `nthRoot` x = fst $ until (uncurry(==)) (\(_,x0) -> (x0,((n-1)*x0+x/x0**(n-1))/n)) (x,x/n)

root :: Loc -> Value -> Eval Value
root l1 argRoot = do
  checkNumArg l1 argRoot
  chain (\l2 argNum -> do
            checkNumArg l2 argNum
            rootFn argRoot argNum)
  where
    rootFn :: Value -> Value -> Eval Value
    rootFn (Number root) (Number n) = return $ Number $ root `nthRoot` n
    rootFn _ _                      = error "fall through root"

floor :: Loc -> Value -> Eval Value
floor _ (Number n) = return $ Number $ fromIntegral $ Prelude.floor n
floor l v          = throwError $ TypeMismatch l "number" v

ceil :: Loc -> Value -> Eval Value
ceil _ (Number n) = return $ Number $ fromIntegral $ Prelude.ceiling n
ceil l v          = throwError $ TypeMismatch l "number" v

sin :: Loc -> Value -> Eval Value
sin _ (Number n) = return $ Number $ Prelude.sin n
sin l v          = throwError $ TypeMismatch l "number" v

cos :: Loc -> Value -> Eval Value
cos _ (Number n) = return $ Number $ Prelude.cos n
cos l v          = throwError $ TypeMismatch l "number" v
