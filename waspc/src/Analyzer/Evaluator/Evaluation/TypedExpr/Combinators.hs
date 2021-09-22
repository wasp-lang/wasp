{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Analyzer.Evaluator.Evaluation.TypedExpr.Combinators
  ( string,
    integer,
    double,
    bool,
    decl,
    enum,
    list,
    extImport,
    json,
    psl,
  )
where

import Analyzer.Evaluator.Decl.Operations (fromDecl)
import Analyzer.Evaluator.Evaluation.Internal (evaluation, evaluation', runEvaluation)
import Analyzer.Evaluator.Evaluation.TypedExpr (TypedExprEvaluation)
import qualified Analyzer.Evaluator.EvaluationError as EvaluationError
import qualified Analyzer.Evaluator.Types as E
import qualified Analyzer.Type as T
import qualified Analyzer.TypeChecker.AST as TypedAST
import qualified Analyzer.TypeDefinitions as TD
import Control.Arrow (left)
import qualified Data.HashMap.Strict as H

-- | An evaluation that expects a "StringLiteral".
string :: TypedExprEvaluation String
string = evaluation' $ \case
  TypedAST.StringLiteral str -> pure str
  expr -> Left $ EvaluationError.ExpectedType T.StringType (TypedAST.exprType expr)

-- | An evaluation that expects an "IntegerLiteral" or "DoubleLiteral". A
-- "DoubleLiteral" is rounded to the nearest whole number.
integer :: TypedExprEvaluation Integer
integer = evaluation' $ \case
  TypedAST.IntegerLiteral i -> pure i
  TypedAST.DoubleLiteral x -> pure $ round x
  expr -> Left $ EvaluationError.ExpectedType T.NumberType (TypedAST.exprType expr)

-- | An evaluation that expects a "IntegerLiteral" or "DoubleLiteral".
double :: TypedExprEvaluation Double
double = evaluation' $ \case
  TypedAST.IntegerLiteral i -> pure $ fromIntegral i
  TypedAST.DoubleLiteral x -> pure x
  expr -> Left $ EvaluationError.ExpectedType T.NumberType (TypedAST.exprType expr)

-- | An evaluation that expects a "BoolLiteral".
bool :: TypedExprEvaluation Bool
bool = evaluation' $ \case
  TypedAST.BoolLiteral b -> pure b
  expr -> Left $ EvaluationError.ExpectedType T.BoolType (TypedAST.exprType expr)

-- | An evaluation that expects a "Var" bound to a "Decl" of type "a".
decl :: forall a. TD.IsDeclType a => TypedExprEvaluation a
decl = evaluation $ \(_, bindings) -> \case
  TypedAST.Var var typ -> case H.lookup var bindings of
    Nothing -> Left $ EvaluationError.UndefinedVariable var
    Just dcl -> case fromDecl @a dcl of
      Nothing ->
        Left $
          EvaluationError.WithContext
            (EvaluationError.ForVariable var)
            (EvaluationError.ExpectedType (T.DeclType declTypeName) typ)
      Just (_dclName, dclValue) -> Right dclValue
  expr -> Left $ EvaluationError.ExpectedType (T.DeclType declTypeName) (TypedAST.exprType expr)
  where
    declTypeName = TD.dtName $ TD.declType @a

-- | An evaluation that expects a "Var" bound to an "EnumType" for "a".
enum :: forall a. TD.IsEnumType a => TypedExprEvaluation a
enum = evaluation' $ \case
  TypedAST.Var var _ -> TD.enumEvaluate @a var
  expr -> Left $ EvaluationError.ExpectedType (T.EnumType $ TD.etName $ TD.enumType @a) (TypedAST.exprType expr)

-- | An evaluation that expects a "List" and runs the inner evaluation on each
-- item in the list.
list :: TypedExprEvaluation a -> TypedExprEvaluation [a]
list elemEvaluation = evaluation $ \(typeDefs, bindings) -> \case
  TypedAST.List values _ ->
    left (EvaluationError.WithContext EvaluationError.InList) $
      mapM (runEvaluation elemEvaluation typeDefs bindings) values
  expr -> Left $ EvaluationError.ExpectedListType $ TypedAST.exprType expr

-- | An evaluation that expects an "ExtImport".
extImport :: TypedExprEvaluation E.ExtImport
extImport = evaluation' $ \case
  TypedAST.ExtImport name file -> pure $ E.ExtImport name file
  expr -> Left $ EvaluationError.ExpectedType T.ExtImportType (TypedAST.exprType expr)

-- | An evaluation that expects a "JSON".
json :: TypedExprEvaluation E.JSON
json = evaluation' $ \case
  TypedAST.JSON str -> pure $ E.JSON str
  expr -> Left $ EvaluationError.ExpectedType (T.QuoterType "json") (TypedAST.exprType expr)

-- | An evaluation that expects a "PSL".
psl :: TypedExprEvaluation E.PSL
psl = evaluation' $ \case
  TypedAST.PSL str -> pure $ E.PSL str
  expr -> Left $ EvaluationError.ExpectedType (T.QuoterType "psl") (TypedAST.exprType expr)
