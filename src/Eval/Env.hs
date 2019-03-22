module Eval.Env where

import qualified Data.Map      as Map

import           Eval.BuiltIn
import           Eval.Value
import           Parser.Syntax

basicState :: EvalState
basicState = EvalState [Map.fromList builtIns]

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
