module Analyzer.Evaluator.Evaluation.TypedExpr
  ( TypedExprEvaluation,
  )
where

import Analyzer.Evaluator.Evaluation.Core (Evaluation)
import qualified Analyzer.TypeChecker.AST as TypedAST

type TypedExprEvaluation a = Evaluation TypedAST.TypedExpr a
