module Wasp.Generator.JsImport
  ( mkJsImportGetterFromExtSrcDir,
    RelDirToExternalCodeDir,
  )
where

import StrongPath (Dir, Dir', Path, Posix, Rel, (</>))
import qualified StrongPath as SP
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.JsImport
  ( JsImport,
    JsImportName (JsImportField, JsImportModule),
    getJsImport,
  )

type RelDirToExternalCodeDir = Path Posix (Rel GeneratedExternalCodeDir) Dir'

-- | Returns a getter function that can be used to get a JsImport from an ExtImport
mkJsImportGetterFromExtSrcDir ::
  Path Posix (Rel ()) (Dir GeneratedExternalCodeDir) ->
  ( RelDirToExternalCodeDir ->
    EI.ExtImport ->
    JsImport
  )
mkJsImportGetterFromExtSrcDir rootDir relDirToExternalCodeDir extImport = getJsImport importPath importName
  where
    userDefinedPath = SP.castRel $ EI.path extImport
    importName = extImportNameToJsImportName $ EI.name extImport
    importPath = SP.castRel $ relDirToExternalCodeDir </> rootDir </> userDefinedPath

extImportNameToJsImportName :: EI.ExtImportName -> JsImportName
extImportNameToJsImportName (EI.ExtImportModule name) = JsImportModule name
extImportNameToJsImportName (EI.ExtImportField name) = JsImportField name
