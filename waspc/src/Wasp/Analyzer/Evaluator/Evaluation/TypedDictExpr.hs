module Wasp.Analyzer.Evaluator.Evaluation.TypedDictExpr
  ( TypedDictExprEvaluation,
    TypedDictEntries (..),
  )
where

import Wasp.Analyzer.Evaluator.Evaluation.Internal (Evaluation)
import qualified Wasp.Analyzer.TypeChecker.AST as TypedAST

-- | A transformation from dictionary definition (which is a list of dictionary entries) to some type. An "Evaluation" can
-- be created from a "TypedDictExprEvaluation" with the "dict" combinator.
type TypedDictExprEvaluation a = Evaluation TypedDictEntries a

-- | This first WithCtx carries the context of the dictionary that contains these entries.
newtype TypedDictEntries = TypedDictEntries (TypedAST.WithCtx [(String, TypedAST.WithCtx TypedAST.TypedExpr)])
