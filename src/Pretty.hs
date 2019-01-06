{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Pretty where

import           Data.List        (intersperse)
import qualified Data.Text.Lazy   as T
import           Prelude          hiding ((<>))
import           Text.PrettyPrint

import           CompilerError
import           Syntax
import           Value

class Pretty p where
  ppr :: Int -> p -> Doc

  {-# INLINE pp #-}
  pp :: p -> Doc
  pp = ppr 0

  {-# INLINE ppg #-}
  ppg :: p -> T.Text
  ppg = T.pack . render . pp

instance Pretty Name where
  ppr _ = text . T.unpack

instance Pretty String where
  ppr _ = text

instance Pretty Int where
  ppr _ x = int x

-- Printer Utils

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

spaced :: Pretty a => Int -> [a] -> Doc
spaced p = hsep . fmap (ppr p)

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

spaces :: Int -> String
spaces n
  | n <= 0    = ""
  | otherwise = replicate n ' '

indent :: Int -> Doc -> Doc
indent i d = hang (text (spaces i) <> d) i empty

commafy :: [Doc] -> Doc
commafy = hsep . punctuate comma

ppmaybe :: Pretty a => Maybe a -> Doc
ppmaybe = maybe empty pp

ppcond :: Bool -> Doc -> Doc -> Doc
ppcond c d1 d2 = if c then d1 else d2

ppif :: Bool -> Doc -> Doc
ppif c d = ppcond c d empty

banner :: String
banner = render $
  text ("\n" ++ ascii ++
  "\n\n Krill 0.1.0\n")
  where
    ascii = " |  ._ o | |\n |< |  | | | "

-- Compiler

instance Pretty CompilerError where
  ppr _ e = case e of
    FileNotFound fname -> "File:" <+> pp fname <+> "not found"
    ReplCommandError s -> pp s
    ParseError s       -> pp s
    EvaluationError e  -> pp e

instance Pretty EvalError where
  ppr _ e = case e of
    TypeMismatch txt val -> "Error Type Mismatch: cannot perform" <+> pp txt <+> "with" <+> pp val
    UnboundVar txt       -> "Error Unbound Variable:" <+> pp txt
    NumArgs n args ->
      "Error Number of Arguments, expected:" <+> integer n <+> "recieved:" <+> integer n
    NotFunction val -> "Error Not a Function:" <+> pp val
    OperatorNotFound n -> "Error Operator `" <> pp n <> "` Not Found"
    Default val          -> "Error Evaluation:" <+> pp val

-- Syntax

instance Pretty Literal where
  ppr _ l = case l of
    LitNumber x   -> ppcond (isInt x) (integer $ round x) (double x)
    LitAtom x     -> ":" <> pp x
    LitChar x     -> quotes $ char x
    LitString x   -> doubleQuotes $ pp x
    LitBool True  -> text "true"
    LitBool False -> text "false"

ppapp :: Int -> Expr -> Doc
ppapp p e = parensIf (p>0) $ ppr p f <+> args
  where
    (f, xs) = viewApp e
    args = sep $ fmap (ppr (p+1)) xs

block :: Doc -> Block -> Doc
block d b@(Block [s]) = d <+> pp b
block d b             = (hang (d <+> lbrace) 2 (pp b)) <> "\n}"

instance Pretty Expr where
  ppr p ex = case ex of
    ELit l          -> ppr p l
    e@(EApp {})     -> ppapp p e
    EBinOp op e1 e2 -> ppr p e1 <+> pp op <+> ppr p e2
    EUnOp op e      -> pp op <> ppr p e
    EVar n -> pp n
    e@(ELam _ b) ->
      parensIf (p>0) $ hsep vars <+> "->" <> pp b
      where vars = fmap pp (viewVars e)
    EParens e -> parens (pp e)

instance Pretty Block where
  ppr _ (Block stmts) = case stmts of
    [s] -> pp s
    ss  -> pss
      where
        pss = vcat (fmap pp ss)

instance Pretty Stmt where
  ppr p s = case s of
    SExpr e  -> ppr p e
    SAss n e -> pp n <+> equals <+> ppr p e

instance Pretty Decl where
  ppr _ d = case d of
    Func n args b ->
      block (pp n <+> hsep (fmap pp args) <+> "->") b

instance Pretty Module where
  ppr _ (Module decls) = vcat (intersperse "" (fmap pp decls))

-- Runtime Values

instance Pretty Value where
  ppr p x = case x of
    Number x   -> ppcond (isInt x) (integer $ round x) (double x)
    Atom x     -> ":" <> pp x
    Char x     -> quotes $ char x
    String x   -> doubleQuotes $ pp x
    Bool True  -> text "true"
    Bool False -> text "false"
    Fun _      -> text "(function)"
    Lambda _ _ -> text "(lambda)"
    Nil        -> text "nil"
