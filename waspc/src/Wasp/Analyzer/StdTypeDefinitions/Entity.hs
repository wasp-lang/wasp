{-# LANGUAGE TypeApplications #-}

module Wasp.Analyzer.StdTypeDefinitions.Entity () where

import qualified Wasp.Analyzer.Evaluator.EvaluationError as EvaluationError
import qualified Wasp.Analyzer.Type as Type
import qualified Wasp.Analyzer.TypeChecker.AST as TC.AST
import Wasp.Analyzer.TypeDefinitions.Class (IsDeclType (..))
-- TODO: Hm why is this import Internal? Should we make it non-Internal then?
import Wasp.Analyzer.TypeDefinitions.Internal (DeclType (..))
import qualified Wasp.AppSpec.Core.Decl as Decl
import Wasp.AppSpec.Entity (Entity)
import qualified Wasp.AppSpec.Entity as Entity

instance IsDeclType Entity where
  declType =
    DeclType
      { dtName = "entity",
        dtBodyType = Type.QuoterType "psl",
        -- dtEvaluate :: TypeDefinitions -> Bindings -> DeclName -> TypedExpr -> Either EvaluationError Decl
        dtEvaluate = \typeDefinitions bindings declName expr ->
          Decl.makeDecl @Entity declName <$> declEvaluate typeDefinitions bindings expr
      }

  -- declEvaluate :: TypeDefinitions -> Bindings -> TypedExpr -> Either EvaluationError a
  declEvaluate typeDefinitions bindings expr = case expr of
    -- TODO: Instead of just passing the string, parse that string into PSL.AST and then
    --   construct Entity from that.
    TC.AST.PSL pslString -> Right $ Entity.Entity $ Entity.PSL pslString
    _ -> Left $ EvaluationError.ExpectedType (Type.QuoterType "psl") (TC.AST.exprType expr)
