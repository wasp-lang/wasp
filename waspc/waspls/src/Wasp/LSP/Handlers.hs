{-# LANGUAGE DataKinds #-}

module Wasp.LSP.Handlers
  ( initializedHandler,
    shutdownHandler,
    didOpenHandler,
    didChangeHandler,
    didSaveHandler,
    completionHandler,
    gotoDefinitionHandler,
  )
where

import Control.Lens ((.~), (?~), (^.))
import Control.Monad (forM_, when, (<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Log.Class (logM)
import Control.Monad.Reader (asks)
import qualified Data.HashMap.Strict as M
import Data.List (stripPrefix)
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Server (Handlers)
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import qualified Language.LSP.VFS as LSP
import qualified StrongPath as SP
import Wasp.Analyzer (analyze)
import Wasp.Analyzer.Parser.ConcreteParser (parseCST)
import qualified Wasp.Analyzer.Parser.Lexer as L
import Wasp.LSP.Completion (getCompletionsAtPosition)
import Wasp.LSP.Debouncer (debounce)
import Wasp.LSP.Diagnostic (WaspDiagnostic (AnalyzerDiagonstic, ParseDiagnostic), waspDiagnosticToLspDiagnostic)
import Wasp.LSP.ExtImport (refreshAllExports, refreshExportsForFiles, updateMissingImportDiagnostics)
import Wasp.LSP.GotoDefinition (gotoDefinitionOfSymbolAtPosition)
import Wasp.LSP.ServerM (HandlerM, ServerM, handler, modify, sendToReactor)
import Wasp.LSP.ServerState (cst, currentWaspSource, latestDiagnostics)
import qualified Wasp.LSP.ServerState as State

-- LSP notification and request handlers

-- | "Initialized" notification is sent when the client is started. We don't
-- have anything we need to do at initialization, but this is required to be
-- implemented.
--
-- The client starts the LSP at its own discretion, but commonly this is done
-- either when:
--
-- - A file of the associated language is opened (in this case `.wasp`).
-- - A workspace is opened that has a project structure associated with the
--   language (in this case, a `main.wasp` file in the root folder of the
--   workspace).
initializedHandler :: Handlers ServerM
initializedHandler = do
  LSP.notificationHandler LSP.SInitialized $ \_params -> do
    -- Register workspace watcher for src/ directory. This is used for checking
    -- TS export lists.
    --
    -- This can fail if the client doesn't support dynamic registration for this:
    -- in that case, we can't provide some features. See "Wasp.LSP.ExtImport" for
    -- what features require this watcher.
    watchSourceFilesToken <-
      LSP.registerCapability
        LSP.SWorkspaceDidChangeWatchedFiles
        LSP.DidChangeWatchedFilesRegistrationOptions
          { _watchers =
              LSP.List
                [LSP.FileSystemWatcher {_globPattern = "**/*.{ts,tsx,js,jsx}", _kind = Nothing}]
          }
        watchSourceFilesHandler
    case watchSourceFilesToken of
      Nothing -> logM "[initializedHandler] Client did not accept WorkspaceDidChangeWatchedFiles registration"
      Just _ -> logM "[initializedHandler] WorkspaceDidChangeWatchedFiles registered for JS/TS source files"
    modify (State.regTokens . State.watchSourceFilesToken .~ watchSourceFilesToken)

-- | Ran when files in src/ change. It refreshes the relevant export lists in
-- the cache and updates missing import diagnostics.
--
-- Both of these tasks are ran in the reactor thread so that other requests
-- can still be answered.
watchSourceFilesHandler :: LSP.Handler ServerM 'LSP.WorkspaceDidChangeWatchedFiles
watchSourceFilesHandler msg = do
  let (LSP.List uris) = fmap (^. LSP.uri) $ msg ^. LSP.params . LSP.changes
  logM $ "[watchSourceFilesHandler] Received file changes: " ++ show uris
  let fileUris = mapMaybe (SP.parseAbsFile <=< stripPrefix "file://" . T.unpack . LSP.getUri) uris
  forM_ fileUris $ \file -> sendToReactor $ do
    -- Refresh export list for modified file
    refreshExportsForFiles [file]
    -- Update diagnostics for the wasp file
    updateMissingImportDiagnostics
    handler $
      asks (^. State.waspFileUri) >>= \case
        Just uri -> do
          logM $ "[watchSourceFilesHandler] Updating missing diagnostics for " ++ show uri
          publishDiagnostics uri
        Nothing -> pure ()

-- | Sent by the client when the client is going to shutdown the server, this
-- is where we do any clean up that needs to be done. This cleanup is:
-- - Stopping the reactor thread
shutdownHandler :: IO () -> Handlers ServerM
shutdownHandler stopReactor = LSP.requestHandler LSP.SShutdown $ \_ resp -> do
  logM "Received shutdown request"
  liftIO stopReactor
  resp $ Right LSP.Empty

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
    completions <- handler $ getCompletionsAtPosition $ request ^. LSP.params . LSP.position
    respond $ Right $ LSP.InL $ LSP.List completions

gotoDefinitionHandler :: Handlers ServerM
gotoDefinitionHandler =
  LSP.requestHandler LSP.STextDocumentDefinition $ \request respond -> do
    definitions <- handler $ gotoDefinitionOfSymbolAtPosition $ request ^. LSP.params . LSP.position
    respond $ Right $ LSP.InR $ LSP.InL definitions

-- | Does not directly handle a notification or event, but should be run when
-- text document content changes.
--
-- It analyzes the document contents and sends any error messages back to the
-- LSP client. In the future, it will also store information about the analyzed
-- file in "Wasp.LSP.State.State".
diagnoseWaspFile :: LSP.Uri -> ServerM ()
diagnoseWaspFile uri = do
  analyzeWaspFile uri

  -- Immediately update import diagnostics only when file watching is enabled
  sourceWatchingEnabled <- isJust <$> handler (asks (^. State.regTokens . State.watchSourceFilesToken))
  when sourceWatchingEnabled updateMissingImportDiagnostics

  -- Send diagnostics to client
  handler $ publishDiagnostics uri

  -- Update exports and missing import diagnostics asynchronously. This is only
  -- done if file watching is NOT enabled or if the export cache hasn't been
  -- filled before.
  exportCacheIsEmpty <- M.null <$> handler (asks (^. State.tsExports))
  debouncer <- handler $ asks (^. State.debouncer)
  when (not sourceWatchingEnabled || exportCacheIsEmpty) $
    debounce debouncer 500000 State.RefreshExports $
      sendToReactor $ do
        refreshAllExports
        updateMissingImportDiagnostics
        handler $ publishDiagnostics uri

publishDiagnostics :: LSP.Uri -> HandlerM ()
publishDiagnostics uri = do
  currentDiagnostics <- asks (^. latestDiagnostics)
  srcString <- asks (^. currentWaspSource)
  let lspDiagnostics = map (waspDiagnosticToLspDiagnostic srcString) currentDiagnostics
  LSP.sendNotification
    LSP.STextDocumentPublishDiagnostics
    $ LSP.PublishDiagnosticsParams uri Nothing (LSP.List lspDiagnostics)

analyzeWaspFile :: LSP.Uri -> ServerM ()
analyzeWaspFile uri = do
  modify (State.waspFileUri ?~ uri)

  -- NOTE: we have to be careful to keep CST and source string in sync at all
  -- times for all threads, so we update them both atomically (via one call to
  -- 'modify').
  readSourceString >>= \case
    Nothing -> do
      logM $ "Couldn't read source from VFS for wasp file " ++ show uri
      pure ()
    Just srcString -> do
      let (concreteErrorMessages, concreteSyntax) = parseCST $ L.lex srcString
      -- Atomic update of source string and CST
      modify ((currentWaspSource .~ srcString) . (cst ?~ concreteSyntax))
      if not $ null concreteErrorMessages
        then storeCSTErrors concreteErrorMessages
        else runWaspAnalyzer srcString
  where
    readSourceString = fmap T.unpack <$> readVFSFile uri

    storeCSTErrors concreteErrorMessages = do
      let newDiagnostics = map ParseDiagnostic concreteErrorMessages
      modify (latestDiagnostics .~ newDiagnostics)

    runWaspAnalyzer srcString = do
      let analyzeResult = analyze srcString
      case analyzeResult of
        Right _ -> do
          modify (latestDiagnostics .~ [])
        Left err -> do
          let newDiagnostics =
                [ AnalyzerDiagonstic err
                ]
          modify (latestDiagnostics .~ newDiagnostics)

-- | Read the contents of a "Uri" in the virtual file system maintained by the
-- LSP library.
readVFSFile :: LSP.Uri -> ServerM (Maybe Text)
readVFSFile uri = fmap LSP.virtualFileText <$> LSP.getVirtualFile (LSP.toNormalizedUri uri)

-- | Get the "Uri" from an object that has a "TextDocument".
extractUri :: (LSP.HasParams a b, LSP.HasTextDocument b c, LSP.HasUri c LSP.Uri) => a -> LSP.Uri
extractUri = (^. (LSP.params . LSP.textDocument . LSP.uri))
