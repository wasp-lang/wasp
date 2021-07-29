module Analyzer.Evaluator.EvaluationError where

newtype EvaluationError = EvaluationError String deriving (Show, Eq)
