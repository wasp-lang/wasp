{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wasp.Analyzer.StdTypeDefinitions.Entity
  ( parsePslBody,
  )
where

import qualified Text.Parsec as Parsec
import Wasp.Analyzer.Evaluator.EvaluationError (mkEvaluationError)
import qualified Wasp.Analyzer.Evaluator.EvaluationError as ER
import qualified Wasp.Analyzer.Type as Type
import qualified Wasp.Analyzer.TypeChecker.AST as TC.AST
import Wasp.Analyzer.TypeDefinitions (DeclType (..), IsDeclType (..))
import qualified Wasp.AppSpec.Core.Decl as Decl
import Wasp.AppSpec.Entity (Entity)
import qualified Wasp.Psl.Ast.Schema as Psl.Ast
import qualified Wasp.Psl.Parser.Model

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

parsePslBody :: String -> Either Parsec.ParseError Psl.Ast.Body
parsePslBody = Parsec.parse Wasp.Psl.Parser.Model.modelBody ""
