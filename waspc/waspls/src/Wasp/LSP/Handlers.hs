module Wasp.LSP.Handlers
  ( initializedHandler,
    didOpenHandler,
    didChangeHandler,
    didSaveHandler,
    completionHandler,
    signatureHelpHandler,
  )
where

import Control.Lens ((.~), (?~), (^.))
import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Server (Handlers)
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import Language.LSP.VFS (virtualFileText)
import Wasp.Analyzer (analyze)
import Wasp.Analyzer.Parser.ConcreteParser (parseCST)
import qualified Wasp.Analyzer.Parser.Lexer as L
import Wasp.LSP.Completion (getCompletionsAtPosition)
import Wasp.LSP.Diagnostic (concreteParseErrorToDiagnostic, waspErrorToDiagnostic)
import Wasp.LSP.ServerM (ServerError (..), ServerM, Severity (..), gets, liftLSP, modify, throwError)
import Wasp.LSP.ServerState (cst, currentWaspSource, latestDiagnostics)
import Wasp.LSP.SignatureHelp (getSignatureHelpAtPosition)

-- LSP notification and request handlers

-- | "Initialized" notification is sent when the client is started. We don't
-- have anything we need to do at initialization, but this is required to be
-- implemented.
--
-- The client starts the LSP at its own discretion, but commonly this is done
-- either when:
--
-- - A file of the associated language is opened (in this case `.wasp`)
-- - A workspace is opened that has a project structure associated with the
--   language (in this case, a `main.wasp` file in the root folder of the
--   workspace)
initializedHandler :: Handlers ServerM
initializedHandler =
  LSP.notificationHandler LSP.SInitialized $ const (return ())

-- | "TextDocumentDidOpen" is sent by the client when a new document is opened.
-- `diagnoseWaspFile` is run to analyze the newly opened document.
didOpenHandler :: Handlers ServerM
didOpenHandler =
  LSP.notificationHandler LSP.STextDocumentDidOpen $ diagnoseWaspFile . extractUri

-- | "TextDocumentDidChange" is sent by the client when a document is changed
-- (i.e. when the user types/deletes text). `diagnoseWaspFile` is run to
-- analyze the changed document.
didChangeHandler :: Handlers ServerM
didChangeHandler =
  LSP.notificationHandler LSP.STextDocumentDidChange $ diagnoseWaspFile . extractUri

-- | "TextDocumentDidSave" is sent by the client when a document is saved.
-- `diagnoseWaspFile` is run to analyze the new contents of the document.
didSaveHandler :: Handlers ServerM
didSaveHandler =
  LSP.notificationHandler LSP.STextDocumentDidSave $ diagnoseWaspFile . extractUri

completionHandler :: Handlers ServerM
completionHandler =
  LSP.requestHandler LSP.STextDocumentCompletion $ \request respond -> do
    completions <- getCompletionsAtPosition $ request ^. LSP.params . LSP.position
    respond $ Right $ LSP.InL $ LSP.List completions

signatureHelpHandler :: Handlers ServerM
signatureHelpHandler =
  LSP.requestHandler LSP.STextDocumentSignatureHelp $ \request respond -> do
    -- NOTE: lsp-types 1.4.0.1 forgot to add lenses for SignatureHelpParams so
    -- we have to get the position out the painful way.
    let LSP.SignatureHelpParams {_position = position} = request ^. LSP.params
    signatureHelp <- getSignatureHelpAtPosition position
    respond $ Right signatureHelp

-- | Does not directly handle a notification or event, but should be run when
-- text document content changes.
--
-- It analyzes the document contents and sends any error messages back to the
-- LSP client. In the future, it will also store information about the analyzed
-- file in "Wasp.LSP.State.State".
diagnoseWaspFile :: LSP.Uri -> ServerM ()
diagnoseWaspFile uri = do
  analyzeWaspFile uri
  currentDiagnostics <- gets (^. latestDiagnostics)
  liftLSP $
    LSP.sendNotification LSP.STextDocumentPublishDiagnostics $
      LSP.PublishDiagnosticsParams uri Nothing (LSP.List currentDiagnostics)

analyzeWaspFile :: LSP.Uri -> ServerM ()
analyzeWaspFile uri = do
  srcString <- readAndStoreSourceString
  let (concreteErrorMessages, concreteSyntax) = parseCST $ L.lex srcString
  modify (cst ?~ concreteSyntax)
  if not $ null concreteErrorMessages
    then storeCSTErrors concreteErrorMessages
    else runWaspAnalyzer srcString
  where
    readAndStoreSourceString = do
      srcString <- T.unpack <$> readVFSFile uri
      modify (currentWaspSource .~ srcString)
      return srcString

    storeCSTErrors concreteErrorMessages = do
      srcString <- gets (^. currentWaspSource)
      newDiagnostics <- mapM (concreteParseErrorToDiagnostic srcString) concreteErrorMessages
      modify (latestDiagnostics .~ newDiagnostics)

    runWaspAnalyzer srcString = do
      let analyzeResult = analyze srcString
      case analyzeResult of
        Right _ -> do
          modify (latestDiagnostics .~ [])
        Left err -> do
          let newDiagnostics =
                [ waspErrorToDiagnostic err
                ]
          modify (latestDiagnostics .~ newDiagnostics)

-- | Read the contents of a "Uri" in the virtual file system maintained by the
-- LSP library.
readVFSFile :: LSP.Uri -> ServerM Text
readVFSFile uri = do
  mVirtualFile <- liftLSP $ LSP.getVirtualFile $ LSP.toNormalizedUri uri
  case mVirtualFile of
    Just virtualFile -> return $ virtualFileText virtualFile
    Nothing -> throwError $ ServerError Error $ "Could not find " <> T.pack (show uri) <> " in VFS."

-- | Get the "Uri" from an object that has a "TextDocument".
extractUri :: (LSP.HasParams a b, LSP.HasTextDocument b c, LSP.HasUri c LSP.Uri) => a -> LSP.Uri
extractUri = (^. (LSP.params . LSP.textDocument . LSP.uri))
