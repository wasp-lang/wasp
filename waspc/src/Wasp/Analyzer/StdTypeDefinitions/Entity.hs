{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wasp.Analyzer.StdTypeDefinitions.Entity () where

import Control.Arrow (left)
import qualified Text.Parsec as Parsec
import qualified Wasp.Analyzer.Evaluator.EvaluationError as EvaluationError
import qualified Wasp.Analyzer.Type as Type
import qualified Wasp.Analyzer.TypeChecker.AST as TC.AST
import Wasp.Analyzer.TypeDefinitions.Class (IsDeclType (..))
-- TODO: Hm why is this import Internal? Should we make it non-Internal then?
--   Or should we move this module into TypeDefinitions?
import Wasp.Analyzer.TypeDefinitions.Internal (DeclType (..))
import qualified Wasp.AppSpec.Core.Decl as Decl
import Wasp.AppSpec.Entity (Entity, makeEntity)
import qualified Wasp.Psl.Parser.Model

instance IsDeclType Entity where
  declType =
    DeclType
      { dtName = "entity",
        dtBodyType = Type.QuoterType "psl",
        dtEvaluate = \typeDefinitions bindings declName expr ->
          Decl.makeDecl @Entity declName <$> declEvaluate typeDefinitions bindings expr
      }

  declEvaluate _ _ expr = case expr of
    TC.AST.PSL pslString ->
      left EvaluationError.ParseError $
        makeEntity <$> Parsec.parse Wasp.Psl.Parser.Model.body "" pslString
    _ -> Left $ EvaluationError.ExpectedType (Type.QuoterType "psl") (TC.AST.exprType expr)
