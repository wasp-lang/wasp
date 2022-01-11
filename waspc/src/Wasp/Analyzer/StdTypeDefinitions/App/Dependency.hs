{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wasp.Analyzer.StdTypeDefinitions.App.Dependency () where

import qualified Wasp.Analyzer.Evaluator.Evaluation as E
import qualified Wasp.Analyzer.Evaluator.EvaluationError as ER
import qualified Wasp.Analyzer.Type as Type
import Wasp.Analyzer.TypeChecker (WithCtx (..))
import qualified Wasp.Analyzer.TypeChecker.AST as TypedAST
import Wasp.Analyzer.TypeDefinitions.Class.HasCustomEvaluation (HasCustomEvaluation (..))
import qualified Wasp.AppSpec.App.Dependency as D

instance HasCustomEvaluation D.Dependency where
  waspType = Type.TupleType (Type.StringType, Type.StringType, [])

  evaluation = E.evaluation' . TypedAST.withCtx $ \ctx texpr -> case texpr of
    TypedAST.Tuple
      ( WithCtx _ (TypedAST.StringLiteral depName),
        WithCtx _ (TypedAST.StringLiteral depVer),
        []
        )
      _ -> return $ D.Dependency {D.name = depName, D.version = depVer}
    _ -> Left $ ER.mkEvaluationError ctx $ ER.ExpectedType (waspType @D.Dependency) (TypedAST.exprType texpr)
