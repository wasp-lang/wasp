module Analyzer.Evaluator.Evaluation.TypedDictExpr
  ( TypedDictExprEvaluation,
    TypedDictEntries (..),
  )
where

import Analyzer.Evaluator.Evaluation.Core (Evaluation)
import qualified Analyzer.TypeChecker.AST as TypedAST

-- | A transformation from dictionary definition (which is a list of dictionary entries) to some type. An "Evaluation" can
-- be created from a "TypedDictExprEvaluation" with the "dict" combinator.
type TypedDictExprEvaluation a = Evaluation TypedDictEntries a

newtype TypedDictEntries = TypedDictEntries [(String, TypedAST.TypedExpr)]
