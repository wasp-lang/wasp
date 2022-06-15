{-# LANGUAGE LambdaCase #-}

module Wasp.LSP.Handlers
  ( initializedHandler,
    didOpenHandler,
    didChangeHandler,
    didSaveHandler,
  )
where

import Control.Lens ((.~), (?~), (^.))
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Server (Handlers, LspT)
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import Language.LSP.VFS (virtualFileText)
import Wasp.Analyzer (analyze)
import Wasp.Backend.ConcreteParser (parseCST)
import Wasp.Backend.ConcreteSyntax (cstPrettyPrint)
import qualified Wasp.Backend.Lexer as L
import Wasp.LSP.Diagnostic (concreteParseErrorToDiagnostic, waspErrorToDiagnostic)
import Wasp.LSP.ServerConfig (ServerConfig)
import Wasp.LSP.ServerM (ServerError (..), ServerM, Severity (..), gets, lift, logM, modify, throwError)
import Wasp.LSP.ServerState (cst, diagnostics)

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

-- | Does not directly handle a notification or event, but should be run when
-- text document content changes.
--
-- It analyzes the document contents and sends any error messages back to the
-- LSP client. In the future, it will also store information about the analyzed
-- file in "Wasp.LSP.State.State".
diagnoseWaspFile :: LSP.Uri -> ServerM ()
diagnoseWaspFile _uri = do
  updateState _uri
  _diagnostics <- gets (^. diagnostics)
  liftLSP $
    LSP.sendNotification LSP.STextDocumentPublishDiagnostics $
      LSP.PublishDiagnosticsParams _uri Nothing (LSP.List _diagnostics)

updateState :: LSP.Uri -> ServerM ()
updateState _uri = do
  src <- readVFSFile _uri
  let srcString = T.unpack src
  let concreteParse = parseCST $ L.lex srcString
  -- Put CST in state
  modify (cst ?~ snd concreteParse)
  logM $ "[updateState] cst=\n" ++ intercalate "\n" (map (("  " ++) . cstPrettyPrint) $ snd concreteParse)
  if not $ null $ fst concreteParse
    then do
      -- Errors found during concrete parsing. Replace diagnostics with new parse
      -- diagnostics.
      _diagnostics <- mapM (concreteParseErrorToDiagnostic srcString) $ fst concreteParse
      modify (diagnostics .~ _diagnostics)
    else do
      -- No concrete parse errors, run full analyzer!
      let analyzeResult = analyze srcString
      case analyzeResult of
        Right _ -> do
          modify (diagnostics .~ [])
        Left err -> do
          let _diagnostics =
                [ waspErrorToDiagnostic err
                ]
          modify (diagnostics .~ _diagnostics)

-- | Run a LSP function in the "ServerM" monad.
liftLSP :: LspT ServerConfig IO a -> ServerM a
liftLSP m = lift (lift m)

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
