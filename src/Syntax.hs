module Syntax where

import qualified Data.Text.Lazy as T

type Name = T.Text

-- Expressions

data Expr
 = EApp Expr Expr        -- a b
 | EBinOp Name Expr Expr -- a + b
 | EUnOp Name Expr       -- !a
 | EVar Name             -- a
 | ELam [Name] [Stmt]    -- x -> x + 1
 | ELit Literal          -- 3
 | EParens Expr          -- (a)
 deriving (Eq, Ord, Show)

-- Literals

data Literal
  = LitNumber Double     -- 1.1
  | LitString T.Text     -- "a"
  | LitChar Char         -- 'a'
  | LitBool Bool         -- true
  deriving (Eq, Ord, Show)

-- Statements

data Stmt
  = SExpr Expr           -- a
  | SAss Name Expr       -- a = b
  | SIf Expr Expr Expr   -- if cond then expr else expr
  deriving (Eq, Ord, Show)

-- Declarations

data Decl
  = Fun Name [Name] [Stmt]
  deriving (Eq, Ord, Show)

-- Module

data Module
  = Module (Maybe Name) [Decl]

-- Helpers

mkEApp :: [Expr] -> Expr
mkEApp = foldl1 EApp
