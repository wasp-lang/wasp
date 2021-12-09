module Wasp.Analyzer.AnalyzeError
  ( AnalyzeError (..),
    getErrorMessage,
    getErrorSourcePosition,
    SourcePosition (..),
  )
where

import qualified Wasp.Analyzer.Evaluator.EvaluationError as EE
import Wasp.Analyzer.Parser (SourcePosition (..))
import qualified Wasp.Analyzer.Parser.ParseError as PE
import qualified Wasp.Analyzer.TypeChecker.TypeError as TE
import Wasp.Util (indent)

data AnalyzeError
  = ParseError PE.ParseError
  | TypeError TE.TypeError
  | EvaluationError EE.EvaluationError
  deriving (Show, Eq)

getErrorMessage :: AnalyzeError -> String
getErrorMessage (ParseError e) = "Parse error:\n" ++ indent 2 (PE.getErrorMessage e)
getErrorMessage (TypeError e) = "Type error:\n" ++ error "TODO"
getErrorMessage (EvaluationError e) = "Evaluation error:\n" ++ error "TODO"

getErrorSourcePosition :: AnalyzeError -> SourcePosition
getErrorSourcePosition (ParseError e) = PE.getSourcePosition e
getErrorSourcePosition (TypeError e) = error "TODO"
getErrorSourcePosition (EvaluationError e) = error "TODO"
