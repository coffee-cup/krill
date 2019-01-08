module CompilerError where

import           Value

data CompilerError
  = FileNotFound FilePath
  | ReplCommandError String
  | ParseError String
  | EvaluationError EvalError
  deriving (Eq)