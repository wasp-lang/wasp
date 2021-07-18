module Analyzer.Evaluator
  ( EvaluationError,
    evaluate,
  )
where

import Analyzer.Decl (Decl)
import Analyzer.TypeChecker (TypedAST)
import Analyzer.TypeDefinitions (TypeDefinitions)

data EvaluationError

-- | Evaluate type-checked AST to produce a list of declarations.
evaluate :: TypeDefinitions -> TypedAST -> Either EvaluationError [Decl]
evaluate _ _ = Right []
