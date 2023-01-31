module Wasp.Generator.ServerGenerator.JsImport where

import Data.Maybe (fromJust)
import qualified StrongPath as SP
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.Generator.JsImport (RelDirToExternalCodeDir, mkJsImportStmtAndIdentifierGetterFromExtSrcDir)
import Wasp.Generator.ServerGenerator.ExternalCodeGenerator (extServerCodeDirInServerSrcDir)
import Wasp.JsImport
  ( JsImportAlias,
    JsImportIdentifier,
    JsImportStatement,
  )

-- Generates JS import statement based on generated external code dir (server)
getJsImportStmtAndIdentifier :: RelDirToExternalCodeDir -> EI.ExtImport -> (JsImportStatement, JsImportIdentifier)
getJsImportStmtAndIdentifier relDirToExternalCodeDir extImport = getJsImportStmtAndIdentifierWithAlias relDirToExternalCodeDir extImport Nothing

getJsImportStmtAndIdentifierWithAlias ::
  RelDirToExternalCodeDir ->
  EI.ExtImport ->
  Maybe JsImportAlias ->
  (JsImportStatement, JsImportIdentifier)
getJsImportStmtAndIdentifierWithAlias = mkJsImportStmtAndIdentifierGetterFromExtSrcDir serverExtDir
  where
    serverExtDir = fromJust (SP.relDirToPosix . SP.castRel $ extServerCodeDirInServerSrcDir)
