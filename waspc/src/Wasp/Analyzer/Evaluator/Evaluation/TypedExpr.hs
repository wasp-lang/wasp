module Wasp.Analyzer.Evaluator.Evaluation.TypedExpr
  ( TypedExprEvaluation,
  )
where

import Wasp.Analyzer.Evaluator.Evaluation.Internal (Evaluation)
import qualified Wasp.Analyzer.TypeChecker.AST as TypedAST

type TypedExprEvaluation a = Evaluation (TypedAST.WithCtx TypedAST.TypedExpr) a
