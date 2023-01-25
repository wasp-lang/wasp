module Wasp.Generator.JsImport
  ( getServerJsImport,
    getClientJsImport,
    getClientJsImportWithAlias,
    getJsImport,
  )
where

import Data.Maybe (fromJust)
import StrongPath (Dir, Dir', Path, Posix, Rel, (</>))
import qualified StrongPath as SP
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.Generator.ServerGenerator.ExternalCodeGenerator (extServerCodeDirInServerSrcDir)
import Wasp.Generator.WebAppGenerator.ExternalCodeGenerator (extClientCodeDirInWebAppSrcDir)
import Wasp.JsImport
  ( JsImportAlias,
    JsImportData,
    JsImportName (JsImportField, JsImportModule),
    JsImportPath,
    getJsImportData,
  )

type RelPathToRoot = Path Posix (Rel GeneratedExternalCodeDir) Dir'

-- | Generates JS import statement based on generated external code dir (server)
getServerJsImport :: RelPathToRoot -> EI.ExtImport -> JsImportData
getServerJsImport relPathToRoot extImport = getServerJsImportWithAlias relPathToRoot extImport Nothing

getServerJsImportWithAlias ::
  RelPathToRoot ->
  EI.ExtImport ->
  Maybe JsImportAlias ->
  JsImportData
getServerJsImportWithAlias = getJsImportFromDir serverExtDir
  where
    serverExtDir = fromJust (SP.relDirToPosix . SP.castRel $ extServerCodeDirInServerSrcDir)

-- | Generates JS import statement based on generated external code dir (web)
getClientJsImport :: RelPathToRoot -> EI.ExtImport -> JsImportData
getClientJsImport relPathToRoot extImport = getClientJsImportWithAlias relPathToRoot extImport Nothing

getClientJsImportWithAlias ::
  RelPathToRoot ->
  EI.ExtImport ->
  Maybe JsImportAlias ->
  JsImportData
getClientJsImportWithAlias = getJsImportFromDir webAppExtDir
  where
    webAppExtDir = fromJust (SP.relDirToPosix . SP.castRel $ extClientCodeDirInWebAppSrcDir)

getJsImportFromDir ::
  Path Posix (Rel ()) (Dir GeneratedExternalCodeDir) ->
  RelPathToRoot ->
  EI.ExtImport ->
  Maybe JsImportAlias ->
  JsImportData
getJsImportFromDir rootDir relPathToRoot extImport importAlias = getJsImportData importPath importName importAlias
  where
    userDefinedPath = SP.castRel $ EI.path extImport
    importName = extImportNameToJsImportName $ EI.name extImport
    importPath = SP.castRel $ relPathToRoot </> rootDir </> userDefinedPath

extImportNameToJsImportName :: EI.ExtImportName -> JsImportName
extImportNameToJsImportName (EI.ExtImportModule name) = JsImportModule name
extImportNameToJsImportName (EI.ExtImportField name) = JsImportField name

-- | Generates JS import statement based on any path
getJsImport :: JsImportPath -> JsImportName -> JsImportData
getJsImport importPath importName = getJsImportData importPath importName Nothing
