module Wasp.Generator.WebAppGenerator.JsImport where

import Data.Maybe (fromJust)
import qualified StrongPath as SP
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.Generator.JsImport (RelDirToExternalCodeDir, mkJsImportGetterFromExtSrcDir)
import Wasp.Generator.WebAppGenerator.ExternalCodeGenerator (extClientCodeDirInWebAppSrcDir)
import Wasp.JsImport
  ( JsImport,
    JsImportIdentifier,
    JsImportStatement,
  )
import qualified Wasp.JsImport as JI

-- | Wrapper function to avoid needing to import getJsImportStmtAndIdentifier from Wasp.JsImport
--  in most cases.
getJsImportStmtAndIdentifier ::
  RelDirToExternalCodeDir ->
  EI.ExtImport ->
  (JsImportStatement, JsImportIdentifier)
getJsImportStmtAndIdentifier relDirToExternalCodeDir = JI.getJsImportStmtAndIdentifier . getJsImport relDirToExternalCodeDir

-- | Generates a JsImport from an ExtImport and relative to web app ext code dir.
getJsImport ::
  RelDirToExternalCodeDir ->
  EI.ExtImport ->
  JsImport
getJsImport = mkJsImportGetterFromExtSrcDir webAppExtDir
  where
    webAppExtDir = fromJust (SP.relDirToPosix . SP.castRel $ extClientCodeDirInWebAppSrcDir)
