module Analyzer
  ( analyze
  ) where

import qualified Analyzer.Evaluator as E
import qualified Analyzer.Parser as P
import qualified Analyzer.TypeChecker as T
import qualified Analyzer.Syntax as S
import Analyzer.StdLib (stdLib)
import Analyzer.Decl (Decl)
import Control.Arrow (left)

data AnalyzeError = ParseError S.ParseError | TypeError T.TypeError | EvaluationError E.EvaluationError

analyze :: String -> Either AnalyzeError [Decl]
analyze source = (left ParseError $ P.parse source) >>=
                 (left TypeError . T.typeCheck stdLib) >>=
                 (left EvaluationError . E.evaluate stdLib)

