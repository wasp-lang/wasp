module Analyzer
  ( analyze
  ) where

import qualified Analyzer.Parser as P
import qualified Analyzer.TypeChecker as T
import qualified Analyzer.Evaluator as E
import Analyzer.StdLib (stdLib)
import Analyzer.Decl (Decl)
import Control.Arrow (left)

data AnalyzeError = ParseError P.ParseError | TypeError T.TypeError | EvaluationError E.EvaluationError

analyze :: String -> Either AnalyzeError [Decl]
analyze source = left ParseError (P.parse source) >>=
                 (left TypeError . T.typeCheck stdLib) >>=
                 (left EvaluationError . E.evaluate stdLib)

