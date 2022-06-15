module Wasp.LSP.Diagnostic
  ( waspErrorToDiagnostic,
    concreteParseErrorToDiagnostic,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.LSP.Types as LSP
import qualified Wasp.Analyzer.AnalyzeError as W
import qualified Wasp.Analyzer.Parser as W
import qualified Wasp.Backend.ParseError as C
import Wasp.LSP.ServerM (ServerM, logM)
import Wasp.LSP.Util (waspSourceRegionToRange)

concreteParseErrorToDiagnostic :: String -> C.ParseError -> ServerM LSP.Diagnostic
concreteParseErrorToDiagnostic src err =
  let _message = Text.pack $ C.showError src err
      _source = "parse"
      _range = concreteErrorRange err
   in logM ("[concreteParseErroToDiagnostic] _range=" ++ show _range)
        >> return
          ( LSP.Diagnostic
              { _range = _range,
                _severity = Nothing,
                _code = Nothing,
                _source = Just _source,
                _message = _message,
                _tags = Nothing,
                _relatedInformation = Nothing
              }
          )
  where
    concreteErrorRange e = case C.errorSpan e of
      C.Span start end ->
        let startPos = C.offsetToSourcePos src start
            endPos = C.offsetToSourcePos src end
         in LSP.Range (concretePosToLSPPos startPos) (concretePosToLSPPos endPos)
    concretePosToLSPPos (C.SourcePos l c) =
      LSP.Position (fromIntegral l - 1) (fromIntegral c - 1)

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
