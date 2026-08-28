{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wasp.Analyzer.StdTypeDefinitions.AuthRequirement () where

import qualified Wasp.Analyzer.Evaluator.Evaluation as E
import qualified Wasp.Analyzer.Evaluator.EvaluationError as ER
import qualified Wasp.Analyzer.Type as Type
import qualified Wasp.Analyzer.TypeChecker.AST as TypedAST
import Wasp.Analyzer.TypeDefinitions.Class.HasCustomEvaluation (HasCustomEvaluation (..))
import Wasp.AppSpec.AuthRequirement (AuthRequirement (..))

-- | 'AuthRequirement' is a union of a boolean and a list of provider ids,
-- which the Template Haskell behind @makeDeclType@ cannot translate. In the
-- classic wasp DSL only the boolean form ever existed (@authRequired: true@),
-- so this instance keeps exactly that working; the provider-restricted form is
-- expressible only in the TypeScript spec, like providers themselves.
instance HasCustomEvaluation AuthRequirement where
  waspType = Type.BoolType

  evaluation = E.evaluation' . TypedAST.withCtx $ \ctx texpr -> case texpr of
    TypedAST.BoolLiteral False -> return AuthNotRequired
    TypedAST.BoolLiteral True -> return AuthRequiredForAnyProvider
    _ ->
      Left $
        ER.mkEvaluationError ctx $
          ER.ExpectedType (waspType @AuthRequirement) (TypedAST.exprType texpr)
