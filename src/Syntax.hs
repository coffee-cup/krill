{-# LANGUAGE BangPatterns #-}

module Syntax where

import qualified Data.Text.Lazy  as T
import           Text.Megaparsec (SourcePos)

type Name = T.Text

data Loc = NoLoc | Located SourcePos
  deriving (Eq, Ord, Show)

-- Expressions

data Expr
 = EApp Loc Expr Expr    -- a b
 | EBinOp Loc Name Expr Expr -- a + b
 | EUnOp Loc Name Expr       -- !a
 | EVar Loc Name             -- a
 | ELam Loc [Name] Block     -- x -> x + 1
 | ELit Loc Literal          -- 3
 | EParens Loc Expr          -- (a)
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
  = SExpr Loc Expr           -- a
  | SAss Loc Name Expr       -- a = b
  | SIf Loc Expr Block Block -- if cond then expr else expr
  deriving (Eq, Ord, Show)

newtype Block = Block [Stmt]
  deriving (Eq, Ord, Show)

-- Declarations

data Decl
  = Func Loc Name [Name] Block
  deriving (Eq, Ord, Show)

-- Module

data Module
  = Module [Decl]
  deriving (Eq, Ord, Show)

-- Helpers

mkEApp :: [Expr] -> Expr
mkEApp = foldl1 (EApp NoLoc)

viewVars :: Expr -> [Name]
viewVars (ELam _ ns _) = ns
viewVars _             = []

viewApp :: Expr -> (Expr, [Expr])
viewApp = go []
  where
    go !xs (EApp _ a b) = go (b : xs) a
    go xs f             = (f, xs)
