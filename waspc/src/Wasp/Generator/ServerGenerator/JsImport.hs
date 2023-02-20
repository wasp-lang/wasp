module Wasp.Generator.ServerGenerator.JsImport where

import Data.Maybe (fromJust)
import StrongPath (Dir, Path, Posix, Rel)
import qualified StrongPath as SP
import qualified Wasp.AppSpec.ExtImport as EI
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.ServerGenerator.Common (ServerSrcDir)
import Wasp.Generator.ServerGenerator.ExternalCodeGenerator (extServerCodeDirInServerSrcDir)
import Wasp.JsImport
  ( JsImport,
    JsImportIdentifier,
    JsImportStatement,
  )
import qualified Wasp.JsImport as JI

getJsImportStmtAndIdentifier ::
  Path Posix (Rel importLocation) (Dir ServerSrcDir) ->
  EI.ExtImport ->
  (JsImportStatement, JsImportIdentifier)
getJsImportStmtAndIdentifier pathFromImportLocationToExtCodeDir = JI.getJsImportStmtAndIdentifier . extImportToJsImport pathFromImportLocationToExtCodeDir

extImportToJsImport ::
  Path Posix (Rel importLocation) (Dir ServerSrcDir) ->
  EI.ExtImport ->
  JsImport
extImportToJsImport = GJI.extImportToJsImport serverExtDir
  where
    serverExtDir = fromJust (SP.relDirToPosix extServerCodeDirInServerSrcDir)
