module Analyzer.Evaluator
  ( EvaluationError
  , evaluate
  ) where

import Analyzer.TypeChecker (TypedAST)
import Analyzer.Decl (Decl)
import Analyzer.Lib (Lib)

data EvaluationError

-- | Evaluate type-checked AST to produce a list of declarations.
evaluate :: Lib -> TypedAST -> Either EvaluationError [Decl]
evaluate _ _ = Right []
