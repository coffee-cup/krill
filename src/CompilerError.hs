module CompilerError where

import           Eval

data CompilerError
  = FileNotFound FilePath
  | ReplCommandError String
  | ParseError String
  | EvalError EvalError
  deriving (Eq)
