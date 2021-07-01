module Analyzer
  ( analyze,
  )
where

import Analyzer.Decl (Decl)
import qualified Analyzer.Evaluator as E
import qualified Analyzer.Parser as P
import Analyzer.StdLib (stdLib)
import qualified Analyzer.TypeChecker as T
import Control.Arrow (left)
import Control.Monad ((>=>))

data AnalyzeError
  = ParseError P.ParseError
  | TypeError T.TypeError
  | EvaluationError E.EvaluationError

-- | Takes a Wasp source file and produces a list of declarations or a
--   description of an error in the source file.
analyze :: String -> Either AnalyzeError [Decl]
analyze =
  (left ParseError . P.parse)
    >=> (left TypeError . T.typeCheck stdLib)
    >=> (left EvaluationError . E.evaluate stdLib)
