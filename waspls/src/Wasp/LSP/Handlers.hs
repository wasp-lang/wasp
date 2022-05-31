module Wasp.LSP.Handlers
  ( initializedHandler,
    didOpenHandler,
    didChangeHandler,
    didSaveHandler,
  )
where

import Control.Lens ((+~), (^.))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (catchE, throwE)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Server (Handlers, LspT)
import qualified Language.LSP.Server as LSP
import Language.LSP.Types
import Language.LSP.Types.Lens
import Language.LSP.VFS (virtualFileText)
import qualified Wasp.Analyzer
import qualified Wasp.Analyzer.AnalyzeError as WE
import Wasp.Analyzer.Parser (Ctx (Ctx))
import Wasp.Analyzer.Parser.SourceRegion (getRgnEnd, getRgnStart)
import Wasp.LSP.State (HandlerM, ServerConfig, Severity (..), State)

liftLSP :: LspT ServerConfig IO a -> HandlerM a
liftLSP m = lift (lift m)

readUri :: Uri -> HandlerM Text
readUri _uri = do
  mVirtualFile <- liftLSP $ LSP.getVirtualFile $ toNormalizedUri _uri
  case mVirtualFile of
    Just virtualFile -> return $ virtualFileText virtualFile
    Nothing -> throwE (Error, "Could not find " <> T.pack (show _uri) <> " in VFS.")

extractUri :: (HasParams a b, HasTextDocument b c, HasUri c Uri) => a -> Uri
extractUri = (^. (params . textDocument . uri))

initializedHandler :: Handlers HandlerM
initializedHandler =
  LSP.notificationHandler SInitialized $ \_ -> return ()

didOpenHandler :: Handlers HandlerM
didOpenHandler =
  LSP.notificationHandler STextDocumentDidOpen $ diagnosticsHandler . extractUri

-- TODO: is performance bad doing this on every change?
didChangeHandler :: Handlers HandlerM
didChangeHandler =
  LSP.notificationHandler STextDocumentDidChange $ diagnosticsHandler . extractUri

didSaveHandler :: Handlers HandlerM
didSaveHandler =
  LSP.notificationHandler STextDocumentDidSave $ diagnosticsHandler . extractUri

diagnosticsHandler :: Uri -> HandlerM ()
diagnosticsHandler _uri = do
  src <- readUri _uri
  let analyzeResult = Wasp.Analyzer.analyze $ T.unpack src
  diags <- case analyzeResult of
    -- Valid wasp file, send no diagnostics
    Right _ -> return $ List []
    -- Report the error (for now, just one error per analyze is possible)
    Left err -> do
      let (errMsg, errSrc, errRange) = getErrorDiagnostics err
      return $
        List
          [ Diagnostic
              { _range = errRange,
                _severity = Nothing,
                _code = Nothing,
                _source = Just errSrc,
                _message = errMsg,
                _tags = Nothing,
                _relatedInformation = Nothing
              }
          ]
  liftLSP $
    LSP.sendNotification STextDocumentPublishDiagnostics $
      PublishDiagnosticsParams _uri Nothing diags
  where
    getErrorDiagnostics :: WE.AnalyzeError -> (Text, Text, Range)
    getErrorDiagnostics err =
      let errSrc = case err of
            WE.ParseError _ -> "parse"
            WE.TypeError _ -> "typecheck"
            WE.EvaluationError _ -> "evaluate"
          (errMsg, errCtx) = WE.getErrorMessageAndCtx err
       in (T.pack errMsg, errSrc, ctxToRange errCtx)

    ctxToRange :: Ctx -> Range
    ctxToRange (Ctx region) =
      Range
        { _start = sourcePositionToPosition (getRgnStart region),
          _end = sourcePositionToPosition (getRgnEnd region) & character +~ (1 :: UInt)
        }

    sourcePositionToPosition (WE.SourcePosition l c) =
      Position (fromIntegral $ l - 1) (fromIntegral $ c - 1)

handleErrorWithDefault :: (Either a b -> HandlerM c) -> b -> HandlerM c -> HandlerM c
handleErrorWithDefault respond _def = flip catchE handler
  where
    handler (Log, _message) = do
      liftLSP $
        LSP.sendNotification SWindowLogMessage $
          LogMessageParams {_xtype = MtLog, _message = _message}
      respond (Right _def)
    handler (_severity, _message) = do
      let _xtype = case _severity of
            Error -> MtError
            Warning -> MtWarning
            Info -> MtInfo
            Log -> MtLog
      liftLSP $
        LSP.sendNotification SWindowShowMessage $
          ShowMessageParams {_xtype = _xtype, _message = _message}
      respond (Right _def)
