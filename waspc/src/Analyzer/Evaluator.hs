module Analyzer.Evaluator
  ( EvaluationError,
    evaluate,
  )
where

import Analyzer.Decl (Decl)
import Analyzer.Lib (Lib)
import Analyzer.TypeChecker (TypedAST)

data EvaluationError

-- | Evaluate type-checked AST to produce a list of declarations.
evaluate :: Lib -> TypedAST -> Either EvaluationError [Decl]
evaluate _ _ = Right []
