{-# LANGUAGE BangPatterns #-}

module Syntax where

import qualified Data.Text.Lazy as T

type Name = T.Text

-- Expressions

data Expr
 = EApp Expr Expr        -- a b
 | EBinOp Name Expr Expr -- a + b
 | EUnOp Name Expr       -- !a
 | EVar Name             -- a
 | ELam [Name] Block     -- x -> x + 1
 | ELit Literal          -- 3
 | EParens Expr          -- (a)
 deriving (Eq, Ord, Show)

-- Literals

data Literal
  = LitNumber Double     -- 1.1
  | LitString T.Text     -- "a"
  | LitChar Char         -- 'a'
  | LitBool Bool         -- true
  | LitAtom T.Text       -- :atom
  deriving (Eq, Ord, Show)

-- Statements

data Stmt
  = SExpr Expr           -- a
  | SAss Name Expr       -- a = b
  | SIf Expr Block Block -- if cond then expr else expr
  deriving (Eq, Ord, Show)

newtype Block = Block [Stmt]
  deriving (Eq, Ord, Show)

-- Declarations

data Decl
  = Func Name [Name] Block
  deriving (Eq, Ord, Show)

-- Module

data Module
  = Module [Decl]
  deriving (Eq, Ord, Show)

-- Helpers

mkEApp :: [Expr] -> Expr
mkEApp = foldl1 EApp

viewVars :: Expr -> [Name]
viewVars (ELam ns _) = ns
viewVars _           = []

viewApp :: Expr -> (Expr, [Expr])
viewApp = go []
  where
    go !xs (EApp a b) = go (b : xs) a
    go xs f           = (f, xs)
