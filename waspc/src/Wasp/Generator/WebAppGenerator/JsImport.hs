module Wasp.Generator.WebAppGenerator.JsImport where

import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import StrongPath (Dir, Path, Posix, Rel)
import qualified StrongPath as SP
import Wasp.AppSpec.ExtImport (ExtImport)
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.Generator.JsImport
  ( jsImportToImportJson,
  )
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.WebAppGenerator.Common (WebAppSrcDir)
import Wasp.Generator.WebAppGenerator.ExternalCodeGenerator (extClientCodeDirInWebAppSrcDir)
import Wasp.JsImport
  ( JsImport,
    JsImportIdentifier,
    JsImportStatement,
  )
import qualified Wasp.JsImport as JI

extImportToImportJson ::
  Path Posix (Rel importLocation) (Dir WebAppSrcDir) ->
  Maybe ExtImport ->
  Aeson.Value
extImportToImportJson pathFromImportLocationToSrcDir maybeExtImport = jsImportToImportJson jsImport
  where
    jsImport = extImportToJsImport pathFromImportLocationToSrcDir <$> maybeExtImport

getJsImportStmtAndIdentifier ::
  Path Posix (Rel importLocation) (Dir WebAppSrcDir) ->
  EI.ExtImport ->
  (JsImportStatement, JsImportIdentifier)
getJsImportStmtAndIdentifier pathFromImportLocationToSrcDir = JI.getJsImportStmtAndIdentifier . extImportToJsImport pathFromImportLocationToSrcDir

extImportToJsImport ::
  Path Posix (Rel importLocation) (Dir WebAppSrcDir) ->
  EI.ExtImport ->
  JsImport
extImportToJsImport = GJI.extImportToJsImport webAppExtDir
  where
    webAppExtDir = fromJust (SP.relDirToPosix extClientCodeDirInWebAppSrcDir)
