{-# LANGUAGE DataKinds #-}

module Wasp.LSP.Handlers
  ( initializedHandler,
    shutdownHandler,
    didOpenHandler,
    didChangeHandler,
    didSaveHandler,
    completionHandler,
  )
where

import Control.Concurrent.STM (atomically, readTVar, writeTChan)
import Control.Lens ((.~), (?~), (^.))
import Control.Monad (forM_, (<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Log.Class (logM)
import Control.Monad.Reader (MonadReader (ask), asks)
import Data.List (stripPrefix)
import Data.Maybe (mapMaybe)
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
import Wasp.LSP.Diagnostic (WaspDiagnostic (AnalyzerDiagonstic, ParseDiagnostic), waspDiagnosticToLspDiagnostic)
import Wasp.LSP.ExtImport (refreshExportsForFile)
import Wasp.LSP.Reactor (ReactorInput (ReactorAction))
import Wasp.LSP.ServerM (ServerM, handler, modify, runRLspM)
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
--
-- TODO(before merge): update missing import diagnostics in the same reactor action
watchSourceFilesHandler :: LSP.Handler ServerM 'LSP.WorkspaceDidChangeWatchedFiles
watchSourceFilesHandler msg = do
  let (LSP.List uris) = fmap (^. LSP.uri) $ msg ^. LSP.params . LSP.changes
  logM $ "[didChangeWatchedFilesHandler] Received file changes: " ++ show uris
  let fileUris = mapMaybe (SP.parseAbsFile <=< stripPrefix "file://" . T.unpack . LSP.getUri) uris
  forM_ fileUris $ \file -> do
    stateTVar <- ask
    env <- LSP.getLspEnv
    let inp = ReactorAction $ LSP.runLspT env $ runRLspM stateTVar (refreshExportsForFile file)
    liftIO $
      atomically $ do
        state <- readTVar stateTVar
        writeTChan (state ^. State.reactorIn) inp

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

-- | Does not directly handle a notification or event, but should be run when
-- text document content changes.
--
-- It analyzes the document contents and sends any error messages back to the
-- LSP client. In the future, it will also store information about the analyzed
-- file in "Wasp.LSP.State.State".
--
-- TODO(before merge): also send missing import diagnostics AND refresh exports
-- and missing import diagnostics for all source files pointed to be ExtImport
-- nodes in the file. Not sure if this belongs here or in analyze? Or a mix?
-- Or somewhere new?
diagnoseWaspFile :: LSP.Uri -> ServerM ()
diagnoseWaspFile uri = do
  analyzeWaspFile uri
  currentDiagnostics <- handler $ asks (^. latestDiagnostics)
  srcString <- handler $ asks (^. currentWaspSource)
  let lspDiagnostics = map (waspDiagnosticToLspDiagnostic srcString) currentDiagnostics
  LSP.sendNotification
    LSP.STextDocumentPublishDiagnostics
    $ LSP.PublishDiagnosticsParams uri Nothing (LSP.List lspDiagnostics)

analyzeWaspFile :: LSP.Uri -> ServerM ()
analyzeWaspFile uri = do
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
