module CompilerError where

import           Eval.Value

data CompilerError
  = FileNotFound FilePath
  | ReplCommandError String
  | ParseError String
  | EvaluationError EvalError
  | StdlibNotFound FilePath
  | StdlibError String
  deriving (Eq)
