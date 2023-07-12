module Wasp.LSP.ExtImport.Diagnostic
  ( updateMissingExtImportDiagnostics,
    getMissingExtImportDiagnostics,
  )
where

import Control.Lens ((%~), (^.))
import Control.Monad.Reader.Class (asks)
import Data.Maybe (catMaybes)
import Wasp.Analyzer.Parser (ExtImportName (ExtImportField, ExtImportModule))
import qualified Wasp.Analyzer.Parser.CST.Traverse as T
import Wasp.LSP.Diagnostic (MissingExtImportReason (..), WaspDiagnostic (MissingExtImportDiagnostic), clearMissingExtImportDiagnostics)
import Wasp.LSP.ExtImport.ExportsCache (ExtImportLookupResult (..), lookupExtImport)
import Wasp.LSP.ExtImport.Syntax (ExtImportNode (einLocation), getAllExtImports)
import Wasp.LSP.ServerMonads (HandlerM, ServerM, handler, modify)
import qualified Wasp.LSP.ServerState as State

-- | Clears missing external import diagnostics and computes new diagnostics to
-- replace them with. Does not publish the diagnostics.
updateMissingExtImportDiagnostics :: ServerM ()
updateMissingExtImportDiagnostics = do
  newDiagnostics <- handler getMissingExtImportDiagnostics
  modify (State.latestDiagnostics %~ ((++ newDiagnostics) . clearMissingExtImportDiagnostics))

-- | Finds missing external import diagnostics for all ExtImports in the current
-- concrete syntax tree.
getMissingExtImportDiagnostics :: HandlerM [WaspDiagnostic]
getMissingExtImportDiagnostics =
  asks (^. State.cst) >>= \case
    Nothing -> return []
    Just syntax -> do
      src <- asks (^. State.currentWaspSource)
      let allExtImports = getAllExtImports src syntax
      catMaybes <$> mapM findDiagnosticForExtImport allExtImports

-- | Check for a diagnostic at a single external import.
findDiagnosticForExtImport :: ExtImportNode -> HandlerM (Maybe WaspDiagnostic)
findDiagnosticForExtImport extImport =
  lookupExtImport extImport >>= \case
    ImportSyntaxError -> return Nothing -- Syntax errors are already reported elsewhere.
    ImportCacheMiss -> return Nothing
    ImportedFileDoesNotExist file -> return $ Just $ MissingExtImportDiagnostic extImportSpan NoFile file
    ImportedSymbolDoesNotExist symbol file -> return $ Just $ diagnosticForExtImport symbol file
    ImportsSymbol _ _ -> return Nothing -- Valid import.
  where
    diagnosticForExtImport (ExtImportModule _) file =
      MissingExtImportDiagnostic extImportSpan NoDefaultExport file
    diagnosticForExtImport (ExtImportField name) file =
      MissingExtImportDiagnostic extImportSpan (NoNamedExport name) file

    extImportSpan = T.spanAt $ einLocation extImport
