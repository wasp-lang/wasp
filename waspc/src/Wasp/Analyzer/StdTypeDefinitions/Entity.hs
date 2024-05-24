{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wasp.Analyzer.StdTypeDefinitions.Entity () where

import Wasp.Analyzer.Evaluator.EvaluationError (mkEvaluationError)
import qualified Wasp.Analyzer.Evaluator.EvaluationError as ER
import qualified Wasp.Analyzer.Type as Type
import qualified Wasp.Analyzer.TypeChecker.AST as TC.AST
import Wasp.Analyzer.TypeDefinitions (DeclType (..), IsDeclType (..))
import qualified Wasp.AppSpec.Core.Decl as Decl
import Wasp.AppSpec.Entity (Entity)

instance IsDeclType Entity where
  declType =
    DeclType
      { dtName = "entity",
        -- TODO: I'm not sure what should be do with this. I'm just returning BoolType for now.
        dtBodyType = Type.BoolType,
        dtEvaluate = \typeDefinitions bindings declName expr ->
          Decl.makeDecl @Entity declName <$> declEvaluate typeDefinitions bindings expr
      }

  declEvaluate _ _ (TC.AST.WithCtx ctx _) = Left $ mkEvaluationError ctx ER.EntitiesNotSupported
