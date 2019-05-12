{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pretty where

import           Data.List        (intersperse)
import qualified Data.Map         as Map
import qualified Data.Text.Lazy   as T
import           Prelude          hiding ((<>))
import           Text.Megaparsec  (sourcePosPretty)
import           Text.PrettyPrint

import           CompilerError
import           Eval.Value
import           Parser.Syntax

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

withLocation :: Loc -> Doc -> Doc
withLocation NoLoc d = d
withLocation (Located pos) d =
  text (sourcePosPretty pos) <+> d

-- Compiler

instance Pretty CompilerError where
  ppr _ e = case e of
    CompilerError.FileNotFound fname ->
      "File" <+> pp fname <+> "not found"
    ReplCommandError s -> pp s
    ParseError s       -> pp s
    EvaluationError e  -> pp e
    StdlibError s      -> pp s <+> "standard library"
    StdlibNotFound p   -> "Could not find standard library " <+> pp p

instance Pretty EvalError where
  ppr _ e = case e of
    TypeMismatch l txt val ->
      withLocation l ("Error: Type Mismatch\n  Expected:"
                      <+> pp txt <+> "\n  Received:" <+> pp val)
    UnboundVar l txt ->
      withLocation l ("Error: Unbound Variable" <+> pp txt)
    NumArgs l expected received ->
      withLocation l ("Error: Number of Arguments, expected:"
                      <+> integer expected
                      <+> "recieved:"
                      <+> integer received)
    NotFunction l val ->
      withLocation l ("Error: Not a Function:" <+> pp val)
    OperatorNotFound l n ->
      withLocation l ("Error: Operator `" <> pp n <> "` Not Found")
    VariableAlreadyBound l n ->
      withLocation l ("Error: Variable `" <> pp n <> "` Already Bound")
    NotAnInteger l v ->
      withLocation l ("Error:" <+> pp v <+> "is not an integer")
    IndexOutOfRange l i ->
      withLocation l ("Error: Index" <+> (integer i) <+> "is out of range")
    NoParse l to v ->
      withLocation l ("Error: Could not parse" <+> pp v <+> "to a" <+> pp to)
    Eval.Value.FileNotFound l fname ->
      withLocation l ("Error: File" <+> pp fname <+> "not found")
    ThrowError l s ->
      withLocation l ("Error:" <+> pp s)
    Default l val ->
      withLocation l ("Error Evaluation:" <+> pp val)

-- Syntax

instance Pretty Literal where
  ppr _ l = case l of
    LitNumber x   -> ppcond (isInt x) (integer $ round x) (double x)
    LitAtom x     -> ":" <> pp x
    LitChar x     -> quotes $ char x
    LitString x   -> doubleQuotes $ pp x
    LitBool True  -> text "true"
    LitBool False -> text "false"
    LitUnit       -> text "()"

ppapp :: Int -> Expr -> Doc
ppapp p e = parensIf (p>0) $ ppr p f <+> args
  where
    (f, xs) = viewApp e
    args = sep $ fmap (ppr (p+1)) xs

block :: Doc -> Block -> Doc
block d b@(Block [_]) = d <+> pp b
block d b             = hang (d <+> lbrace) 2 (pp b) <> "\n}"

instance Pretty Expr where
  ppr p ex = case ex of
    ELit _ l          -> ppr p l
    e@EApp {}     -> ppapp p e
    EBinOp _ op e1 e2 -> ppr p e1 <+> pp op <+> ppr p e2
    EUnOp _ op e      -> pp op <> ppr p e
    EVar _ n -> pp n
    ELam _ args b ->
      parensIf (p>0) $ block (hsep (fmap pp args) <+> "->") b
    EList _ xs -> text "[" <> hsep pxs <> text "]"
      where pxs = punctuate (text ",") $ fmap pp xs
    EIf _ c t e
      -> block (block (text "if" <+> pp c <+> "then") t) e
    EFor _ n xs b ->
      block (text "for" <+> pp n <+> "in" <+> pp xs) b
    EAss _ n e -> pp n <+> equals <+> ppr p e
    EListAcc _ n e -> pp n <> text "[" <> pp e <> text "]"
    ERange _ s n e -> case n of
      ELit _ LitUnit -> text "[" <> pp s <> text ".." <> pp e <> text "]"
      eNext -> text "[" <> pp s <> text "," <> pp eNext <> text ".." <> pp e <> text "]"
    EParens _ e -> parens (pp e)

instance Pretty Block where
  ppr _ (Block stmts) = case stmts of
    [s] -> pp s
    ss  -> pss
      where
        pss = vcat (fmap pp ss)

instance Pretty Stmt where
  ppr p s = case s of
    SExpr _ e  -> ppr p e

instance Pretty Decl where
  ppr _ d = case d of
    Func _ n args b ->
      block (pp n <+> hsep (fmap pp args) <+> "->") b

instance Pretty Module where
  ppr _ (Module decls) = vcat (intersperse "" (fmap pp decls))

-- Runtime Values

instance Pretty Value where
  ppr _ x = case x of
    Number x   -> ppcond (isInt x) (integer $ round x) (double x)
    Atom x     -> ":" <> pp x
    Char x     -> quotes $ char x
    String x   -> doubleQuotes $ pp x
    Bool True  -> text "true"
    Bool False -> text "false"
    Lambda _ _ -> text "(lambda)"
    BuiltIn _  -> text "(lambda)"
    List xs ->
      text "[" <> hcat pxs <> text "]"
      where pxs = punctuate (text ",") $ fmap pp xs
    Unit       -> text "()"

instance Show Value where
  show = T.unpack . ppg

instance Pretty Env where
  ppr _ env = vcat (fmap ppscope env)
    where
      ppscope :: Map.Map T.Text Value -> Doc
      ppscope s =
        (vcat $ fmap (\(k, v) -> pp k <+> "->" <+> pp v) (Map.toList s)) <+> "\n~\n"

