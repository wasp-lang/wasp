{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wasp.Analyzer.StdTypeDefinitions.App.AuthProvider () where

import qualified Data.HashMap.Strict as H
import qualified Wasp.Analyzer.Evaluator.Evaluation as E
import qualified Wasp.Analyzer.Evaluator.EvaluationError as ER
import qualified Wasp.Analyzer.Type as Type
import Wasp.Analyzer.TypeChecker.AST (withCtx)
import Wasp.Analyzer.TypeDefinitions.Class.HasCustomEvaluation (HasCustomEvaluation (..))
import qualified Wasp.AppSpec.App.Auth as Auth

-- | 'Auth.AuthProviderSpec' carries fields (an Either-backed server entry, a
-- map of extensions) that the Template Haskell behind @makeDeclType ''App@
-- cannot translate. This instance satisfies it by hand.
--
-- The classic wasp DSL cannot express an auth provider at all (providers are
-- constructed by spec helpers like @waspAuth()@ from @\@wasp.sh/auth/spec@,
-- which only exist in the TypeScript spec), so the evaluation's only honest
-- behavior is to say so.
instance HasCustomEvaluation Auth.AuthProviderSpec where
  waspType = Type.DictType H.empty

  evaluation = E.evaluation' . withCtx $ \ctx _texpr ->
    Left $
      ER.mkEvaluationError ctx $
        ER.ParseError $
          ER.EvaluationParseError
            "app.auth.providers cannot be expressed in the classic wasp DSL. Use the TypeScript spec (main.wasp.ts)."
