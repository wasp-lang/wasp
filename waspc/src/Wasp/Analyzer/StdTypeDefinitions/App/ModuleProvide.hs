{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wasp.Analyzer.StdTypeDefinitions.App.ModuleProvide () where

import qualified Wasp.Analyzer.Evaluator.Evaluation as E
import qualified Wasp.Analyzer.Evaluator.EvaluationError as ER
import qualified Wasp.Analyzer.Type as Type
import qualified Wasp.Analyzer.TypeChecker.AST as TypedAST
import Wasp.Analyzer.TypeDefinitions.Class.HasCustomEvaluation (HasCustomEvaluation (..))
import Wasp.AppSpec.App (ModuleProvide)

-- | ModuleProvide is never constructed from the Wasp DSL (.wasp files).
-- It only comes from the TypeScript config (main.wasp.ts) via JSON deserialization.
-- This instance exists so that makeDeclType ''App can process the App record.
instance HasCustomEvaluation ModuleProvide where
  waspType = Type.StringType

  evaluation = E.evaluation' . TypedAST.withCtx $ \ctx _texpr ->
    Left $
      ER.mkEvaluationError ctx $
        ER.ParseError $
          ER.EvaluationParseError
            "ModuleProvide cannot be constructed from the Wasp DSL"
