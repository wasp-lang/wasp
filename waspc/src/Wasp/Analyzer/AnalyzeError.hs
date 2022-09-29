module Wasp.Analyzer.AnalyzeError
  ( AnalyzeError (..),
    getErrorMessageAndCtx,
    SourcePosition (..),
  )
where

import Control.Arrow (first)
import qualified Wasp.Analyzer.Evaluator.EvaluationError as EE
import Wasp.Analyzer.Parser (Ctx, SourcePosition (..))
import qualified Wasp.Analyzer.Parser.ParseError as PE
import qualified Wasp.Analyzer.TypeChecker.TypeError as TE
import Wasp.Util (indent)

data AnalyzeError
  = ParseError PE.ParseError
  | TypeError TE.TypeError
  | EvaluationError EE.EvaluationError
  deriving (Show, Eq)

getErrorMessageAndCtx :: AnalyzeError -> (String, Ctx)
getErrorMessageAndCtx = \case
  ParseError e -> first (("Parse error:\n" ++) . indent 2) $ PE.getErrorMessageAndCtx e
  TypeError e -> first (("Type error:\n" ++) . indent 2) $ TE.getErrorMessageAndCtx e
  EvaluationError e -> first (("Evaluation error:\n" ++) . indent 2) $ EE.getErrorMessageAndCtx e
