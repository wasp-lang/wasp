module Wasp.Generator.JsImport
  ( getServerJsImport,
    getClientJsImport,
    getClientJsImportWithAlias,
    getJsImport,
  )
where

import Data.Maybe (fromJust)
import StrongPath (Dir, Dir', File', Path, Posix, Rel, (</>))
import qualified StrongPath as SP
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.Generator.ServerGenerator.ExternalCodeGenerator (extServerCodeDirInServerSrcDir)
import Wasp.Generator.WebAppGenerator.ExternalCodeGenerator (extClientCodeDirInWebAppSrcDir)
import Wasp.JsImport
  ( JsImport (..),
    JsImportAlias,
    JsImportData,
    JsImportName (JsImportField, JsImportModule),
    getJsImportData,
  )

getServerJsImport :: Path Posix (Rel GeneratedExternalCodeDir) Dir' -> EI.ExtImport -> JsImportData
getServerJsImport relPathToRoot extImport = getServerJsImportWithAlias relPathToRoot extImport Nothing

getClientJsImport :: Path Posix (Rel GeneratedExternalCodeDir) Dir' -> EI.ExtImport -> JsImportData
getClientJsImport relPathToRoot extImport = getClientJsImportWithAlias relPathToRoot extImport Nothing

getServerJsImportWithAlias ::
  Path Posix (Rel GeneratedExternalCodeDir) Dir' ->
  EI.ExtImport ->
  Maybe JsImportAlias ->
  JsImportData
getServerJsImportWithAlias = extImportToJsImport serverExtDir
  where
    serverExtDir = fromJust (SP.relDirToPosix . SP.castRel $ extServerCodeDirInServerSrcDir)

getClientJsImportWithAlias ::
  Path Posix (Rel GeneratedExternalCodeDir) Dir' ->
  EI.ExtImport ->
  Maybe JsImportAlias ->
  JsImportData
getClientJsImportWithAlias = extImportToJsImport webAppExtDir
  where
    webAppExtDir = fromJust (SP.relDirToPosix . SP.castRel $ extClientCodeDirInWebAppSrcDir)

extImportToJsImport ::
  Path Posix (Rel ()) (Dir GeneratedExternalCodeDir) ->
  Path Posix (Rel GeneratedExternalCodeDir) Dir' ->
  EI.ExtImport ->
  Maybe JsImportAlias ->
  JsImportData
extImportToJsImport rootDir relPathToRoot extImport importAlias = getJsImportWithAlias importPath importName importAlias
  where
    userDefinedPath = SP.castRel $ EI.path extImport
    importName = extImportNameToJsImportName $ EI.name extImport
    importPath = SP.castRel $ relPathToRoot </> rootDir </> userDefinedPath

getJsImport :: Path Posix (Rel Dir') File' -> JsImportName -> JsImportData
getJsImport importPath importName = getJsImportWithAlias importPath importName Nothing

getJsImportWithAlias :: Path Posix (Rel Dir') File' -> JsImportName -> Maybe JsImportAlias -> JsImportData
getJsImportWithAlias importPath importName importAlias =
  getJsImportData
    JsImport
      { _name = importName,
        _path = importPath,
        _importAlias = importAlias
      }

extImportNameToJsImportName :: EI.ExtImportName -> JsImportName
extImportNameToJsImportName (EI.ExtImportModule name) = JsImportModule name
extImportNameToJsImportName (EI.ExtImportField name) = JsImportField name
