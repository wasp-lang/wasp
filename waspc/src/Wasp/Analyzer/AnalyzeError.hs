module Wasp.Analyzer.AnalyzeError
  ( AnalyzeError (..),
    getErrorMessageAndCtx,
    SourcePosition (..),
  )
where

import Control.Arrow (first)
import Wasp.Analyzer.Ctx (Ctx)
import qualified Wasp.Analyzer.Evaluator.EvaluationError as EE
import Wasp.Analyzer.SourcePosition (SourcePosition (..))
import qualified Wasp.Analyzer.TypeChecker.TypeError as TE
import Wasp.Util (indent)

data AnalyzeError
  = TypeError TE.TypeError
  | EvaluationError EE.EvaluationError
  deriving (Show, Eq)

getErrorMessageAndCtx :: AnalyzeError -> (String, Ctx)
getErrorMessageAndCtx = \case
  TypeError e -> first (("Type error:\n" ++) . indent 2) $ TE.getErrorMessageAndCtx e
  EvaluationError e -> first (("Evaluation error:\n" ++) . indent 2) $ EE.getErrorMessageAndCtx e
