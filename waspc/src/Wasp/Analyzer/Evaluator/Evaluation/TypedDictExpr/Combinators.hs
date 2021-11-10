{-# LANGUAGE LambdaCase #-}

module Wasp.Analyzer.Evaluator.Evaluation.TypedDictExpr.Combinators
  ( dict,
    field,
    maybeField,
  )
where

import Wasp.Analyzer.Evaluator.Evaluation.Internal (evaluation, runEvaluation)
import Wasp.Analyzer.Evaluator.Evaluation.TypedDictExpr (TypedDictEntries (..), TypedDictExprEvaluation)
import Wasp.Analyzer.Evaluator.Evaluation.TypedExpr (TypedExprEvaluation)
import qualified Wasp.Analyzer.Evaluator.EvaluationError as EvaluationError
import qualified Wasp.Analyzer.TypeChecker.AST as TypedAST
import Control.Arrow (left)

-- | An evaluation that runs a "TypedDictExprEvaluation". Expects a "Dict" expression and
-- uses its entries to run the "TypedDictExprEvaluation".
dict :: TypedDictExprEvaluation a -> TypedExprEvaluation a
dict dictEvalutor = evaluation $ \(typeDefs, bindings) -> \case
  TypedAST.Dict entries _ -> runEvaluation dictEvalutor typeDefs bindings $ TypedDictEntries entries
  expr -> Left $ EvaluationError.ExpectedDictType $ TypedAST.exprType expr

-- | A dictionary evaluation that requires the field to exist.
field :: String -> TypedExprEvaluation a -> TypedDictExprEvaluation a
field key valueEvaluation = evaluation $
  \(typeDefs, bindings) (TypedDictEntries entries) -> case lookup key entries of
    Nothing -> Left $ EvaluationError.MissingField key
    Just value ->
      left (EvaluationError.WithContext (EvaluationError.InField key)) $
        runEvaluation valueEvaluation typeDefs bindings value

-- | A dictionary evaluation that allows the field to be missing.
maybeField :: String -> TypedExprEvaluation a -> TypedDictExprEvaluation (Maybe a)
maybeField key valueEvaluation = evaluation $
  \(typeDefs, bindings) (TypedDictEntries entries) -> case lookup key entries of
    Nothing -> pure Nothing
    Just value ->
      Just
        <$> left
          (EvaluationError.WithContext (EvaluationError.InField key))
          (runEvaluation valueEvaluation typeDefs bindings value)
