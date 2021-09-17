{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Analyzer.Evaluator.Evaluation.Core
  ( runEvaluation,
    evaluation,
    evaluation',
    Evaluation,
  )
where

import Analyzer.Evaluator.Decl
import Analyzer.Evaluator.EvaluationError (EvaluationError)
import qualified Analyzer.TypeDefinitions as TD
import Data.Functor.Compose (Compose (Compose, getCompose))
import qualified Data.HashMap.Strict as H

-- | An evaluation of "a" into "b". It has evaluation context and it can return an evaluation error.
-- We are using `Compose` because it results in an Applicative when it composes two Applicatives,
-- meaning that our evaluators automatically become instance of Applicative (and Functor),
-- which is then useful for combining multiple evaluations together.
newtype Evaluation a b = Evaluation (Compose ((->) EvalCtx) (Compose ((->) a) (Either EvaluationError)) b)
  deriving (Functor, Applicative)

type EvalCtx =
  -- | Evaluation context.
  (TD.TypeDefinitions, Bindings)

-- TODO: Extract these into Analyzer.Evaluator.Bindings?
type Bindings =
  -- | Declarations evaluated so far.
  H.HashMap DeclName Decl

type DeclName = String

evaluation :: (EvalCtx -> a -> Either EvaluationError b) -> Evaluation a b
evaluation f = Evaluation $ Compose $ \ctx -> Compose $ f ctx

evaluation' :: (a -> Either EvaluationError b) -> Evaluation a b
evaluation' = evaluation . const

runEvaluation :: Evaluation a b -> TD.TypeDefinitions -> Bindings -> a -> Either EvaluationError b
runEvaluation (Evaluation f) typeDefs bindings = getCompose (getCompose f (typeDefs, bindings))
