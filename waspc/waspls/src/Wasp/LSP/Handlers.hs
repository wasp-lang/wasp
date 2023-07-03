{-# LANGUAGE DataKinds #-}

module Wasp.LSP.Handlers
  ( initializedHandler,
    shutdownHandler,
    didOpenHandler,
    didChangeHandler,
    didSaveHandler,
    executeCommandHandler,
    completionHandler,
    signatureHelpHandler,
    gotoDefinitionHandler,
    codeActionHandler,
  )
where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Log.Class (logM)
import Language.LSP.Server (Handlers)
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import Wasp.LSP.Analysis (diagnoseWaspFile)
import Wasp.LSP.CodeActions (getCodeActionsInRange)
import qualified Wasp.LSP.Command as Command
import Wasp.LSP.Completion (getCompletionsAtPosition)
import Wasp.LSP.DynamicHandlers (registerDynamicCapabilities)
import Wasp.LSP.GotoDefinition (gotoDefinitionOfSymbolAtPosition)
import Wasp.LSP.ServerMonads (ServerM, handler)
import Wasp.LSP.SignatureHelp (getSignatureHelpAtPosition)

-- | "Initialized" notification is sent when the client is started. We send
-- all of our dynamic capability registration requests when this happens.
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
    registerDynamicCapabilities

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

executeCommandHandler :: Handlers ServerM
executeCommandHandler = Command.handler

completionHandler :: Handlers ServerM
completionHandler =
  LSP.requestHandler LSP.STextDocumentCompletion $ \request respond -> do
    completions <- handler $ getCompletionsAtPosition $ request ^. LSP.params . LSP.position
    respond $ Right $ LSP.InL $ LSP.List completions

gotoDefinitionHandler :: Handlers ServerM
gotoDefinitionHandler =
  LSP.requestHandler LSP.STextDocumentDefinition $ \request respond -> do
    definitions <- handler $ gotoDefinitionOfSymbolAtPosition $ request ^. LSP.params . LSP.position
    respond $ Right $ LSP.InR $ LSP.InR definitions

signatureHelpHandler :: Handlers ServerM
signatureHelpHandler =
  LSP.requestHandler LSP.STextDocumentSignatureHelp $ \request respond -> do
    -- NOTE: lsp-types 1.4.0.1 forgot to add lenses for SignatureHelpParams so
    -- we have to get the position out the painful way.
    let LSP.SignatureHelpParams {_position = position} = request ^. LSP.params
    signatureHelp <- handler $ getSignatureHelpAtPosition position
    respond $ Right signatureHelp

codeActionHandler :: Handlers ServerM
codeActionHandler =
  LSP.requestHandler LSP.STextDocumentCodeAction $ \request respond -> do
    let range = request ^. LSP.params . LSP.range
    codeActions <- handler $ getCodeActionsInRange range
    respond $ Right $ LSP.List $ map LSP.InR codeActions

-- | Get the 'Uri' from an object that has a 'TextDocument'.
extractUri :: (LSP.HasParams a b, LSP.HasTextDocument b c, LSP.HasUri c LSP.Uri) => a -> LSP.Uri
extractUri = (^. (LSP.params . LSP.textDocument . LSP.uri))
