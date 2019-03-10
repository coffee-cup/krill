{-# LANGUAGE BangPatterns #-}

module Syntax where

import qualified Data.Text.Lazy  as T
import           Text.Megaparsec (SourcePos)

type Name = T.Text

data Loc = NoLoc | Located SourcePos
  deriving (Eq, Ord, Show)

-- Expressions

data Expr
 = EApp Loc Expr Expr        -- a b
 | EBinOp Loc Name Expr Expr -- a + b
 | EUnOp Loc Name Expr       -- !a
 | EVar Loc Name             -- a
 | ELam Loc [Name] Block     -- x -> x + 1
 | ELit Loc Literal          -- 3
 | EIf Loc Expr Block Block  -- if cond then expr else expr
 | EAss Loc Name Expr        -- a = b
 | EList Loc [Expr]          -- [1, x, "hello"]
 | EParens Loc Expr          -- (a)
 deriving (Ord, Show)

-- Literals

data Literal
  = LitNumber Double     -- 1.1
  | LitString T.Text     -- "a"
  | LitChar Char         -- 'a'
  | LitBool Bool         -- true
  | LitAtom T.Text       -- :atom
  | LitUnit              -- ()
  deriving (Eq, Ord, Show)

-- Statements

data Stmt
  = SExpr Loc Expr           -- a
  deriving (Ord, Show)

newtype Block = Block [Stmt]
  deriving (Eq, Ord, Show)

-- Declarations

data Decl
  = Func Loc Name [Name] Block
  deriving (Eq, Ord, Show)

-- Module

data Module
  = Module [Stmt]
  deriving (Eq, Ord, Show)

-- Helpers

class Location a where
  loc :: a -> Loc

class (Location a) => CompareWithoutLocation a where
  eqNoLoc :: a -> a -> Bool

instance Location Expr where
  loc e = case e of
    EApp l _ _     -> l
    EBinOp l _ _ _ -> l
    EUnOp l _ _    -> l
    EVar l _       -> l
    ELam l _ _     -> l
    ELit l _       -> l
    EIf l _ _ _    -> l
    EAss l _ _     -> l
    EParens l _    -> l

instance Location Stmt where
  loc s = case s of
    SExpr l _  -> l

instance Eq Expr where
  (==) (EApp _ e1 e2) (EApp _ e3 e4) =
    e1 == e3 && e2 == e4
  (==) (EBinOp _ n1 e1 e2) (EBinOp _ n2 e3 e4) =
    n1 == n2 && e1 == e3 && e2 == e4
  (==) (EUnOp _ n1 e1) (EUnOp _ n2 e2) =
    n1 == n2 && e1 == e2
  (==) (EVar _ n1) (EVar _ n2) =
    n1 == n2
  (==) (ELam _ ns1 b1) (ELam _ ns2 b2) =
    ns1 == ns2 && b1 == b2
  (==) (ELit _ l1) (ELit _ l2) =
    l1 == l2
  (==) (EIf _ c1 e1 e2) (EIf _ c2 e3 e4) =
    c1 == c2 && e1 == e3 && e2 == e4
  (==) (EAss _ e1 e2) (EAss _ e3 e4) =
    e1 == e3 && e2 == e4
  (==) (EParens _ e1) (EParens _ e2) =
    e1 == e2
  (==) _ _ = False

instance Eq Stmt where
  (==) (SExpr _ e1) (SExpr _ e2) =
    e1 == e2

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
