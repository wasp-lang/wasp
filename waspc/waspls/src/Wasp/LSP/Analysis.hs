module Wasp.LSP.Analysis
  ( diagnoseWaspFile,
    publishDiagnostics,
  )
where

import Control.Lens ((.~), (?~), (^.))
import Control.Monad (when)
import Control.Monad.Log.Class (logM)
import Control.Monad.Reader.Class (asks)
import qualified Data.HashMap.Strict as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.VFS as LSP
import Wasp.Analyzer (analyze)
import Wasp.Analyzer.Parser.ConcreteParser (parseCST)
import qualified Wasp.Analyzer.Parser.Lexer as L
import Wasp.LSP.Debouncer (debounce)
import Wasp.LSP.Diagnostic (WaspDiagnostic (AnalyzerDiagnostic, ParseDiagnostic), waspDiagnosticToLspDiagnostic)
import Wasp.LSP.ExtImport.Diagnostic (updateMissingExtImportDiagnostics)
import Wasp.LSP.ExtImport.ExportsCache (refreshExportsForAllExtImports)
import Wasp.LSP.ServerMonads (HandlerM, ServerM, handler, modify, sendToReactor)
import qualified Wasp.LSP.ServerState as State

-- | Finds diagnostics on a wasp file and sends the diagnostics to the LSP
-- client.
diagnoseWaspFile :: LSP.Uri -> ServerM ()
diagnoseWaspFile uri = do
  analyzeWaspFile uri

  -- Immediately update import diagnostics only when file watching is enabled
  sourceWatchingEnabled <- isJust <$> handler (asks (^. State.regTokens . State.watchSourceFilesToken))
  when sourceWatchingEnabled updateMissingExtImportDiagnostics

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
        refreshExportsForAllExtImports
        updateMissingExtImportDiagnostics
        handler $ publishDiagnostics uri

-- | Send diagnostics stored in 'State.latestDiagnostics' to the LSP client.
publishDiagnostics :: LSP.Uri -> HandlerM ()
publishDiagnostics uri = do
  currentDiagnostics <- asks (^. State.latestDiagnostics)
  srcString <- asks (^. State.currentWaspSource)
  let lspDiagnostics = map (waspDiagnosticToLspDiagnostic srcString) currentDiagnostics
  LSP.sendNotification
    LSP.STextDocumentPublishDiagnostics
    $ LSP.PublishDiagnosticsParams uri Nothing (LSP.List lspDiagnostics)

-- | Run wasp Analyzer on a file and replace the diagnostics in 'State.latestDiagnostics'
-- with the diagnostics reported by the Analyzer.
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
      modify ((State.currentWaspSource .~ srcString) . (State.cst ?~ concreteSyntax))
      if not $ null concreteErrorMessages
        then storeCSTErrors concreteErrorMessages
        else runWaspAnalyzer srcString
  where
    readSourceString = fmap T.unpack <$> readVFSFile uri

    storeCSTErrors concreteErrorMessages = do
      let newDiagnostics = map ParseDiagnostic concreteErrorMessages
      modify (State.latestDiagnostics .~ newDiagnostics)

    runWaspAnalyzer srcString = do
      let analyzeResult = analyze srcString
      case analyzeResult of
        Right _ -> do
          modify (State.latestDiagnostics .~ [])
        Left errs -> do
          let newDiagnostics = fmap AnalyzerDiagnostic errs
          modify (State.latestDiagnostics .~ newDiagnostics)

-- | Read the contents of a "Uri" in the virtual file system maintained by the
-- LSP library.
readVFSFile :: LSP.Uri -> ServerM (Maybe Text)
readVFSFile uri = fmap LSP.virtualFileText <$> LSP.getVirtualFile (LSP.toNormalizedUri uri)
