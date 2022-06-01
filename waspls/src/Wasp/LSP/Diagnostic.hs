module Wasp.LSP.Diagnostic (waspErrorToDiagnostic) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.LSP.Types as LSP
import qualified Wasp.Analyzer.AnalyzeError as W
import qualified Wasp.Analyzer.Parser as W
import Wasp.LSP.Util (waspSourceRegionToRange)

waspErrorToDiagnostic :: W.AnalyzeError -> LSP.Diagnostic
waspErrorToDiagnostic err =
  let _message = waspErrorMessage err
      _source = waspErrorSource err
      _range = waspErrorRange err
   in LSP.Diagnostic
        { _range = _range,
          _severity = Nothing,
          _code = Nothing,
          _source = Just _source,
          _message = _message,
          _tags = Nothing,
          _relatedInformation = Nothing
        }

-- | Convert a wasp error to a message to display to the developer.
--
-- TODO: Write a new conversion from error to text here that is better suited
-- for in-editor display
waspErrorMessage :: W.AnalyzeError -> Text
waspErrorMessage = Text.pack . fst . W.getErrorMessageAndCtx

waspErrorSource :: W.AnalyzeError -> Text
waspErrorSource (W.ParseError _) = "parse"
waspErrorSource (W.TypeError _) = "typecheck"
waspErrorSource (W.EvaluationError _) = "evaluate"

waspErrorRange :: W.AnalyzeError -> LSP.Range
waspErrorRange err =
  let (_, W.Ctx rgn) = W.getErrorMessageAndCtx err
   in waspSourceRegionToRange rgn
