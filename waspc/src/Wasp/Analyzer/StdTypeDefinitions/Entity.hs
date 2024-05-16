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
import qualified Wasp.Psl.Ast.Model
import qualified Wasp.Psl.Parser.Model

instance IsDeclType Entity where
  declType =
    DeclType
      { dtName = "entity",
        dtBodyType = Type.QuoterType "psl",
        dtEvaluate = \typeDefinitions bindings declName expr ->
          Decl.makeDecl @Entity declName <$> declEvaluate typeDefinitions bindings expr
      }

  declEvaluate _ _ (TC.AST.WithCtx ctx expr) = Left $ mkEvaluationError ctx $ ER.ExpectedType (Type.QuoterType "psl") (TC.AST.exprType expr)

-- TC.AST.PSL pslString ->
--   left (ER.mkEvaluationError ctx . ER.ParseError . ER.EvaluationParseErrorParsec) $
--     makeEntity <$> parsePslBody pslString
-- _ -> Left $ mkEvaluationError ctx $ ER.ExpectedType (Type.QuoterType "psl") (TC.AST.exprType expr)

parsePslBody :: String -> Either Parsec.ParseError Wasp.Psl.Ast.Model.Body
parsePslBody = Parsec.parse Wasp.Psl.Parser.Model.body ""
