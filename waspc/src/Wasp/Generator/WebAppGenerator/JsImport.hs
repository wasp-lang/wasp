module Wasp.Generator.WebAppGenerator.JsImport where

import Data.Maybe (fromJust)
import qualified StrongPath as SP
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.Generator.JsImport (RelDirToExternalCodeDir, mkJsImportStmtAndIdentifierGetterFromExtSrcDir)
import Wasp.Generator.WebAppGenerator.ExternalCodeGenerator (extClientCodeDirInWebAppSrcDir)
import Wasp.JsImport
  ( JsImportAlias,
    JsImportIdentifier,
    JsImportStatement,
  )

-- | Generates JS import statement based on generated external code dir (web)
getJsImportStmtAndIdentifier :: RelDirToExternalCodeDir -> EI.ExtImport -> (JsImportStatement, JsImportIdentifier)
getJsImportStmtAndIdentifier relDirToExternalCodeDir extImport = getJsImportStmtAndIdentifierWithAlias relDirToExternalCodeDir extImport Nothing

getJsImportStmtAndIdentifierWithAlias ::
  RelDirToExternalCodeDir ->
  EI.ExtImport ->
  Maybe JsImportAlias ->
  (JsImportStatement, JsImportIdentifier)
getJsImportStmtAndIdentifierWithAlias = mkJsImportStmtAndIdentifierGetterFromExtSrcDir webAppExtDir
  where
    webAppExtDir = fromJust (SP.relDirToPosix . SP.castRel $ extClientCodeDirInWebAppSrcDir)
