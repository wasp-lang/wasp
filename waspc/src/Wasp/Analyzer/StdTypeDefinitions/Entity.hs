{-# LANGUAGE TypeApplications #-}

module Wasp.Analyzer.StdTypeDefinitions.Entity () where

-- TODO: Hm why is this import Internal? Should we make it non-Internal then?

import Control.Arrow (left)
import qualified Text.Parsec as Parsec
import qualified Wasp.Analyzer.Evaluator.EvaluationError as EvaluationError
import qualified Wasp.Analyzer.Type as Type
import qualified Wasp.Analyzer.TypeChecker.AST as TC.AST
import Wasp.Analyzer.TypeDefinitions.Class (IsDeclType (..))
import Wasp.Analyzer.TypeDefinitions.Internal (DeclType (..))
import qualified Wasp.AppSpec.Core.Decl as Decl
import Wasp.AppSpec.Entity (Entity)
import qualified Wasp.AppSpec.Entity as Entity
import qualified Wasp.Psl.Parser.Model

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
  declEvaluate _ _ expr = case expr of
    TC.AST.PSL pslString ->
      left EvaluationError.ParseError $
        Entity.Entity . Entity.PSL
          -- TODO: Update Entity so it contains PSL model and not just String.
          -- TODO: Consider moving this "parse" function to Wasp.Psl.Parser, therefore hiding that
          --   Parsec is used.
          <$> Parsec.parse Wasp.Psl.Parser.Model.body "" pslString
    _ -> Left $ EvaluationError.ExpectedType (Type.QuoterType "psl") (TC.AST.exprType expr)
