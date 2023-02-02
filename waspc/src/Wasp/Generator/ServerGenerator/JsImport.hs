module Wasp.Generator.ServerGenerator.JsImport where

import Data.Maybe (fromJust)
import qualified StrongPath as SP
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.Generator.JsImport (RelDirToExternalCodeDir)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.ServerGenerator.ExternalCodeGenerator (extServerCodeDirInServerSrcDir)
import Wasp.JsImport
  ( JsImport,
    JsImportIdentifier,
    JsImportStatement,
  )
import qualified Wasp.JsImport as JI

getJsImportStmtAndIdentifier ::
  RelDirToExternalCodeDir ->
  EI.ExtImport ->
  (JsImportStatement, JsImportIdentifier)
getJsImportStmtAndIdentifier relDirToExternalCodeDir = JI.getJsImportStmtAndIdentifier . extImportToJsImport relDirToExternalCodeDir

extImportToJsImport ::
  RelDirToExternalCodeDir ->
  EI.ExtImport ->
  JsImport
extImportToJsImport = GJI.extImportToJsImport serverExtDir
  where
    serverExtDir = fromJust (SP.relDirToPosix . SP.castRel $ extServerCodeDirInServerSrcDir)
