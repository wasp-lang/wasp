{-# LANGUAGE LambdaCase #-}

module Wasp.Analyzer.Evaluator.Evaluation.TypedDictExpr.Combinators
  ( dict,
    field,
    maybeField,
  )
where

import Control.Arrow (left)
import Wasp.Analyzer.Evaluator.Evaluation.Internal (evaluation, runEvaluation)
import Wasp.Analyzer.Evaluator.Evaluation.TypedDictExpr (TypedDictEntries (..), TypedDictExprEvaluation)
import Wasp.Analyzer.Evaluator.Evaluation.TypedExpr (TypedExprEvaluation)
import Wasp.Analyzer.Evaluator.EvaluationError (mkEvaluationError)
import qualified Wasp.Analyzer.Evaluator.EvaluationError as ER
import Wasp.Analyzer.TypeChecker.AST (WithCtx (..), withCtx)
import qualified Wasp.Analyzer.TypeChecker.AST as TypedAST

-- | An evaluation that runs a "TypedDictExprEvaluation". Expects a "Dict" expression and
-- uses its entries to run the "TypedDictExprEvaluation".
dict :: TypedDictExprEvaluation a -> TypedExprEvaluation a
dict dictEvaluator = evaluation $ \(typeDefs, bindings) -> withCtx $ \ctx -> \case
  TypedAST.Dict entries _ ->
    runEvaluation dictEvaluator typeDefs bindings $ TypedDictEntries $ WithCtx ctx entries
  expr -> Left $ mkEvaluationError ctx $ ER.ExpectedDictType $ TypedAST.exprType expr

-- | A dictionary evaluation that requires the field to exist.
field :: String -> TypedExprEvaluation a -> TypedDictExprEvaluation a
field key valueEvaluation = evaluation $
  \(typeDefs, bindings) (TypedDictEntries (WithCtx dictCtx entries)) -> case lookup key entries of
    Nothing -> Left $ mkEvaluationError dictCtx $ ER.MissingDictField key
    Just value ->
      left (mkEvaluationError dictCtx . ER.WithEvalErrorCtx (ER.InField key)) $
        runEvaluation valueEvaluation typeDefs bindings value

-- | A dictionary evaluation that allows the field to be missing.
maybeField :: String -> TypedExprEvaluation a -> TypedDictExprEvaluation (Maybe a)
maybeField key valueEvaluation = evaluation $
  \(typeDefs, bindings) (TypedDictEntries (WithCtx dictCtx entries)) -> case lookup key entries of
    Nothing -> pure Nothing
    Just value ->
      Just
        <$> left
          (mkEvaluationError dictCtx . ER.WithEvalErrorCtx (ER.InField key))
          (runEvaluation valueEvaluation typeDefs bindings value)
