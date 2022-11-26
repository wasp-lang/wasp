{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Wasp.Analyzer.Evaluator.Evaluation.TypedExpr.Combinators
  ( string,
    integer,
    double,
    bool,
    declRef,
    enum,
    list,
    extImport,
    json,
    tuple2,
    tuple3,
    tuple4,
  )
where

import Control.Applicative ((<|>))
import Control.Arrow (left)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.UTF8 as ByteStringLazyUTF8
import Data.List (stripPrefix)
import qualified StrongPath as SP
import Wasp.Analyzer.Evaluator.Evaluation.Internal (evaluation, evaluation', runEvaluation)
import Wasp.Analyzer.Evaluator.Evaluation.TypedExpr (TypedExprEvaluation)
import qualified Wasp.Analyzer.Evaluator.EvaluationError as ER
import qualified Wasp.Analyzer.Type as T
import Wasp.Analyzer.TypeChecker.AST (withCtx)
import qualified Wasp.Analyzer.TypeChecker.AST as TypedAST
import qualified Wasp.Analyzer.TypeDefinitions as TD
import Wasp.AppSpec.Core.Ref (Ref)
import qualified Wasp.AppSpec.Core.Ref as Ref
import qualified Wasp.AppSpec.ExtImport as AppSpec.ExtImport
import qualified Wasp.AppSpec.JSON as AppSpec.JSON

-- | An evaluation that expects a "StringLiteral".
string :: TypedExprEvaluation String
string = evaluation' . withCtx $ \ctx -> \case
  TypedAST.StringLiteral str -> pure str
  expr -> Left $ ER.mkEvaluationError ctx $ ER.ExpectedType T.StringType (TypedAST.exprType expr)

-- | An evaluation that expects an "IntegerLiteral" or "DoubleLiteral". A
-- "DoubleLiteral" is rounded to the nearest whole number.
integer :: TypedExprEvaluation Integer
integer = evaluation' . withCtx $ \ctx -> \case
  TypedAST.IntegerLiteral i -> pure i
  TypedAST.DoubleLiteral x -> pure $ round x
  expr -> Left $ ER.mkEvaluationError ctx $ ER.ExpectedType T.NumberType (TypedAST.exprType expr)

-- | An evaluation that expects a "IntegerLiteral" or "DoubleLiteral".
double :: TypedExprEvaluation Double
double = evaluation' . withCtx $ \ctx -> \case
  TypedAST.IntegerLiteral i -> pure $ fromIntegral i
  TypedAST.DoubleLiteral x -> pure x
  expr -> Left $ ER.mkEvaluationError ctx $ ER.ExpectedType T.NumberType (TypedAST.exprType expr)

-- | An evaluation that expects a "BoolLiteral".
bool :: TypedExprEvaluation Bool
bool = evaluation' . withCtx $ \ctx -> \case
  TypedAST.BoolLiteral b -> pure b
  expr -> Left $ ER.mkEvaluationError ctx $ ER.ExpectedType T.BoolType (TypedAST.exprType expr)

-- | An evaluation that expects a "Var" bound to a "Decl" of type "a".
declRef :: forall a. TD.IsDeclType a => TypedExprEvaluation (Ref a)
declRef = evaluation' . withCtx $ \ctx -> \case
  TypedAST.Var varName varType ->
    case varType of
      T.DeclType declTypeName | declTypeName == expectedDeclTypeName -> pure $ Ref.Ref varName
      _ ->
        Left $
          ER.mkEvaluationError ctx $
            ER.WithEvalErrorCtx
              (ER.ForVariable varName)
              (ER.mkEvaluationError ctx $ ER.ExpectedType expectedType varType)
  expr ->
    Left $
      ER.mkEvaluationError ctx $
        ER.ExpectedType expectedType (TypedAST.exprType expr)
  where
    expectedDeclTypeName = TD.dtName $ TD.declType @a
    expectedType = T.DeclType expectedDeclTypeName

-- | An evaluation that expects a "Var" bound to an "EnumType" for "a".
enum :: forall a. TD.IsEnumType a => TypedExprEvaluation a
enum = evaluation' . withCtx $ \ctx -> \case
  TypedAST.Var identifier _ -> case TD.enumEvaluate @a identifier of
    Nothing -> Left $ ER.mkEvaluationError ctx $ ER.InvalidEnumVariant enumName enumVariants identifier
    Just v -> Right v
  expr ->
    Left $
      ER.mkEvaluationError ctx $
        ER.ExpectedType (T.EnumType enumName) (TypedAST.exprType expr)
  where
    enumName = TD.etName $ TD.enumType @a
    enumVariants = TD.etVariants $ TD.enumType @a

-- | An evaluation that expects a "List" and runs the inner evaluation on each
-- item in the list.
list :: TypedExprEvaluation a -> TypedExprEvaluation [a]
list elemEvaluation = evaluation $ \(typeDefs, bindings) -> withCtx $ \ctx -> \case
  TypedAST.List values _ ->
    left (ER.mkEvaluationError ctx . ER.WithEvalErrorCtx ER.InList) $
      mapM (runEvaluation elemEvaluation typeDefs bindings) values
  expr -> Left $ ER.mkEvaluationError ctx $ ER.ExpectedListType $ TypedAST.exprType expr

-- | An evaluation that expects a "Tuple" with 2 elements (pair) and runs the
-- corresponding evaluation on each element.
tuple2 ::
  TypedExprEvaluation t1 ->
  TypedExprEvaluation t2 ->
  TypedExprEvaluation (t1, t2)
tuple2 eval1 eval2 = evaluation $ \(typeDefs, bindings) -> withCtx $ \ctx -> \case
  TypedAST.Tuple (v1, v2, []) _ ->
    left (ER.mkEvaluationError ctx . ER.WithEvalErrorCtx ER.InTuple) $ do
      v1' <- runEvaluation eval1 typeDefs bindings v1
      v2' <- runEvaluation eval2 typeDefs bindings v2
      return (v1', v2')
  expr -> Left $ ER.mkEvaluationError ctx $ ER.ExpectedTupleType 2 $ TypedAST.exprType expr

-- | An evaluation that expects a "Tuple" with 3 elements (triple) and runs the
-- corresponding evaluation on each element.
tuple3 ::
  TypedExprEvaluation t1 ->
  TypedExprEvaluation t2 ->
  TypedExprEvaluation t3 ->
  TypedExprEvaluation (t1, t2, t3)
tuple3 eval1 eval2 eval3 = evaluation $ \(typeDefs, bindings) -> withCtx $ \ctx -> \case
  TypedAST.Tuple (v1, v2, [v3]) _ ->
    left (ER.mkEvaluationError ctx . ER.WithEvalErrorCtx ER.InTuple) $ do
      v1' <- runEvaluation eval1 typeDefs bindings v1
      v2' <- runEvaluation eval2 typeDefs bindings v2
      v3' <- runEvaluation eval3 typeDefs bindings v3
      return (v1', v2', v3')
  expr -> Left $ ER.mkEvaluationError ctx $ ER.ExpectedTupleType 3 $ TypedAST.exprType expr

-- | An evaluation that expects a "Tuple" with 4 elements and runs the
-- corresponding evaluation on each element.
tuple4 ::
  TypedExprEvaluation t1 ->
  TypedExprEvaluation t2 ->
  TypedExprEvaluation t3 ->
  TypedExprEvaluation t4 ->
  TypedExprEvaluation (t1, t2, t3, t4)
tuple4 eval1 eval2 eval3 eval4 = evaluation $ \(typeDefs, bindings) -> withCtx $ \ctx -> \case
  TypedAST.Tuple (v1, v2, [v3, v4]) _ ->
    left (ER.mkEvaluationError ctx . ER.WithEvalErrorCtx ER.InTuple) $ do
      v1' <- runEvaluation eval1 typeDefs bindings v1
      v2' <- runEvaluation eval2 typeDefs bindings v2
      v3' <- runEvaluation eval3 typeDefs bindings v3
      v4' <- runEvaluation eval4 typeDefs bindings v4
      return (v1', v2', v3', v4')
  expr -> Left $ ER.mkEvaluationError ctx $ ER.ExpectedTupleType 4 $ TypedAST.exprType expr

-- | An evaluation that expects an "ExtImport".
extImport :: TypedExprEvaluation AppSpec.ExtImport.ExtImport
extImport = evaluation' . withCtx $ \ctx -> \case
  TypedAST.ExtImport name extImportPath ->
    -- NOTE(martin): This parsing here could instead be done in Parser.
    --   I don't have a very good reason for doing it here instead of Parser, except
    --   for being somewhat simpler to implement.
    --   So we might want to move it to Parser at some point in the future, if we
    --   figure out that is better (it sounds/feels like it could be).
    case stripImportPrefix extImportPath of
      Just relFileFP -> case SP.parseRelFileP relFileFP of
        Left err -> mkParseError ctx $ show err
        Right relFileSP -> pure $ AppSpec.ExtImport.ExtImport name relFileSP
      Nothing ->
        mkParseError
          ctx
          $ "Path in external import must start with \"" ++ serverPrefix ++ "\"" ++ " or \"" ++ clientPrefix ++ "\"!"
  expr -> Left $ ER.mkEvaluationError ctx $ ER.ExpectedType T.ExtImportType (TypedAST.exprType expr)
  where
    mkParseError ctx msg = Left $ ER.mkEvaluationError ctx $ ER.ParseError $ ER.EvaluationParseError msg
    stripImportPrefix importPath = stripPrefix serverPrefix importPath <|> stripPrefix clientPrefix importPath
    serverPrefix = "@server/"
    clientPrefix = "@client/"

-- | An evaluation that expects a "JSON".
json :: TypedExprEvaluation AppSpec.JSON.JSON
json = evaluation' . withCtx $ \ctx -> \case
  -- TODO: Consider moving String to Aeson conversion into the Parser, so we have better typed info earlier.
  TypedAST.JSON str -> either (Left . jsonParseError ctx) (Right . AppSpec.JSON.JSON) (Aeson.eitherDecode $ ByteStringLazyUTF8.fromString str)
  expr -> Left $ ER.mkEvaluationError ctx $ ER.ExpectedType (T.QuoterType "json") (TypedAST.exprType expr)
  where
    jsonParseError ctx errMsg = ER.mkEvaluationError ctx $ ER.ParseError $ ER.EvaluationParseError $ "Unable to parse JSON. Details: " ++ errMsg
