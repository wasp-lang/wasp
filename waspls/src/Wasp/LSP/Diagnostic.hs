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
import qualified Wasp.Analyzer.Parser.ConcreteParser.ParseError as CPE
import Wasp.Analyzer.Parser.Ctx (getCtxRgn)
import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (..), sourceOffsetToPosition)
import Wasp.Analyzer.Parser.SourceSpan (SourceSpan (..))
import Wasp.LSP.ServerM (ServerM, logM)
import Wasp.LSP.Util (waspSourceRegionToLspRange)

concreteParseErrorToDiagnostic :: String -> CPE.ParseError -> ServerM LSP.Diagnostic
concreteParseErrorToDiagnostic src err =
  let message = Text.pack $ showConcreteParseError src err
      source = "parse"
      range = concreteErrorRange err
   in logM ("[concreteParseErroToDiagnostic] _range=" ++ show range)
        >> return
          ( LSP.Diagnostic
              { _range = range,
                _severity = Nothing,
                _code = Nothing,
                _source = Just source,
                _message = message,
                _tags = Nothing,
                _relatedInformation = Nothing
              }
          )
  where
    concreteErrorRange e = case CPE.errorSpan e of
      SourceSpan startOffset endOffset ->
        let startPos = sourceOffsetToPosition src startOffset
            endPos = sourceOffsetToPosition src endOffset
         in LSP.Range (concretePosToLSPPos startPos) (concretePosToLSPPos endPos)
    concretePosToLSPPos (SourcePosition l c) =
      LSP.Position (fromIntegral l - 1) (fromIntegral c - 1)
    showConcreteParseError :: String -> CPE.ParseError -> String
    showConcreteParseError source e =
      let (msg, ctx) = CPE.getErrorMessageAndCtx source e
       in "Parse error at " ++ show (getCtxRgn ctx) ++ ":\n  " ++ msg

waspErrorToDiagnostic :: W.AnalyzeError -> LSP.Diagnostic
waspErrorToDiagnostic err =
  let message = waspErrorAsPrettyEditorMessage err
      source = waspErrorSource err
      range = waspErrorRange err
   in LSP.Diagnostic
        { _range = range,
          _severity = Nothing,
          _code = Nothing,
          _source = Just source,
          _message = message,
          _tags = Nothing,
          _relatedInformation = Nothing
        }

-- | Convert a wasp error to a message to display to the developer.
--
-- TODO: Write a new conversion from error to text here that is better suited
-- for in-editor display
waspErrorAsPrettyEditorMessage :: W.AnalyzeError -> Text
waspErrorAsPrettyEditorMessage = Text.pack . fst . W.getErrorMessageAndCtx

waspErrorSource :: W.AnalyzeError -> Text
waspErrorSource (W.ParseError _) = "parse"
waspErrorSource (W.TypeError _) = "typecheck"
waspErrorSource (W.EvaluationError _) = "evaluate"

waspErrorRange :: W.AnalyzeError -> LSP.Range
waspErrorRange err =
  let (_, W.Ctx rgn) = W.getErrorMessageAndCtx err
   in waspSourceRegionToLspRange rgn
