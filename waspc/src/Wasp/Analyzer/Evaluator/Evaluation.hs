-- | This module contains combinators for building evaluations to convert
-- TypeChecker.AST into Wasp AST (Wasp.hs), plus runner to execute these evaluations.
--
-- In this context, evaluation is a function that takes a piece of TypeChecker.AST,
-- some additional context, and returns a piece of Wasp AST (or error if evaluation fails).
--
-- Evaluation combinator is a function that takes some arguments and returns evaluation.
-- In other words, an evaluation builder.
--
-- This module exposes all the combinators that are needed to build evaluations to parse the
-- whole TypeChecker.AST into Wasp AST.
--
-- Since our evaluation code is created automatically from Wasp AST by the TH functions (TH.hs),
-- these combinators are mostly used there, in the evaluation functions that TH functions generate.
--
-- Evaluation is an instance of Applicative in order to allow easy composing of multiple evaluations.
--
-- An example of usage, where we are building an evalutor @page@ that evalutes a piece of TypeChecker.AST
-- into a @Page@, which is part of the Wasp AST:
--
-- @
-- data Page = Page { title :: String, author :: Maybe String, content :: String }
--
-- page :: Evaluation Page
-- page = dict $ Page <$> field "title" string <*> maybeField "author" string <*> field "content" string
-- @
--
-- This evaluation would turn the Wasp expression
-- @{ title: "Home", content: "Hello world" }@ into
-- @Page { title = "Home", author = Nothing, content = "Hello world" }@
module Analyzer.Evaluator.Evaluation
  ( runEvaluation,
    module Analyzer.Evaluator.Evaluation.Combinators,
  )
where

import Analyzer.Evaluator.Evaluation.Combinators
import Analyzer.Evaluator.Evaluation.Internal (runEvaluation)
