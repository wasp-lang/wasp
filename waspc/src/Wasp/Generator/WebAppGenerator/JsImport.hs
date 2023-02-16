module Wasp.Generator.WebAppGenerator.JsImport where

import Data.Maybe (fromJust)
import StrongPath (Dir, Path, Posix, Rel)
import qualified StrongPath as SP
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.Generator.JsImport (ImportLocation)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.WebAppGenerator.Common (WebAppSrcDir)
import Wasp.Generator.WebAppGenerator.ExternalCodeGenerator (extClientCodeDirInWebAppSrcDir)
import Wasp.JsImport
  ( JsImport,
    JsImportIdentifier,
    JsImportStatement,
  )
import qualified Wasp.JsImport as JI

getJsImportStmtAndIdentifier ::
  Path Posix (Rel ImportLocation) (Dir WebAppSrcDir) ->
  EI.ExtImport ->
  (JsImportStatement, JsImportIdentifier)
getJsImportStmtAndIdentifier pathFromImportLocationToSrcDir = JI.getJsImportStmtAndIdentifier . extImportToJsImport pathFromImportLocationToSrcDir

extImportToJsImport ::
  Path Posix (Rel ImportLocation) (Dir WebAppSrcDir) ->
  EI.ExtImport ->
  JsImport
extImportToJsImport = GJI.extImportToJsImport webAppExtDir
  where
    webAppExtDir = fromJust (SP.relDirToPosix extClientCodeDirInWebAppSrcDir)
