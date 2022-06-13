module Wasp.LSP.Handlers
  ( initializedHandler,
    didOpenHandler,
    didChangeHandler,
    didSaveHandler,
  )
where

import Control.Lens ((+~), (^.))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (throwE)
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
import Wasp.LSP.State (HandlerM, ServerConfig, Severity (..))

-- LSP notification and request handlers

-- | "Initialized" notification is sent when the client is started. We don't
-- have anything we need to do at initialization, but this is required to be
-- implemented.
initializedHandler :: Handlers HandlerM
initializedHandler =
  LSP.notificationHandler SInitialized $ \_ -> return ()

-- | "TextDocumentDidOpen" is sent by the client when a new document is opened.
-- This handler then runs the `diagnosticsHandler`.
didOpenHandler :: Handlers HandlerM
didOpenHandler =
  LSP.notificationHandler STextDocumentDidOpen $ diagnosticsHandler . extractUri

-- | "TextDocumentDidChange" is sent by the client when a document is changed
-- (i.e. when the user types/deletes text). This handler then runs the
-- `diagnosticsHandler`.
didChangeHandler :: Handlers HandlerM
didChangeHandler =
  LSP.notificationHandler STextDocumentDidChange $ diagnosticsHandler . extractUri

-- | "TextDocumentDidSave" is sent by the client when a document is saved. This
-- handler then runs the `diagnosticsHandler`.
didSaveHandler :: Handlers HandlerM
didSaveHandler =
  LSP.notificationHandler STextDocumentDidSave $ diagnosticsHandler . extractUri

-- | Does not directly handle a notification or event, but should be run
-- text document content changes.
--
-- It analyzes the document contents and sends any error messages back to the
-- LSP client. In the future, it will also store information about the analyzed
-- file in "Wasp.LSP.State.State".
diagnosticsHandler :: Uri -> HandlerM ()
diagnosticsHandler _uri = do
  src <- readVFSFile _uri
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
          -- Increment end character by 1: Wasp uses an inclusive convention for
          -- the end position, but LSP considers end position to not be part of
          -- the range.
          _end = sourcePositionToPosition (getRgnEnd region) & character +~ (1 :: UInt)
        }

    sourcePositionToPosition (WE.SourcePosition l c) =
      Position (fromIntegral $ l - 1) (fromIntegral $ c - 1)

-- | Run a LSP function in the "HandlerM" monad.
liftLSP :: LspT ServerConfig IO a -> HandlerM a
liftLSP m = lift (lift m)

-- | Read the contents of a "Uri" in the virtual file system maintained by the
-- LSP library.
readVFSFile :: Uri -> HandlerM Text
readVFSFile _uri = do
  mVirtualFile <- liftLSP $ LSP.getVirtualFile $ toNormalizedUri _uri
  case mVirtualFile of
    Just virtualFile -> return $ virtualFileText virtualFile
    Nothing -> throwE (Error, "Could not find " <> T.pack (show _uri) <> " in VFS.")

-- | Get the "Uri" from an object that has a "TextDocument".
extractUri :: (HasParams a b, HasTextDocument b c, HasUri c Uri) => a -> Uri
extractUri = (^. (params . textDocument . uri))
