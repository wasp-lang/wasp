module Wasp.Generator.JsImport
  ( extImportToJsImport,
    PathFromImportLocationToExtCodeDir,
  )
where

import StrongPath (Dir, Dir', Path, Posix, Rel, (</>))
import qualified StrongPath as SP
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.Generator.Common (GeneratedCodeSourceDir)
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.JsImport
  ( JsImport,
    JsImportName (JsImportField, JsImportModule),
    makeJsImport,
  )

type PathFromImportLocationToExtCodeDir = Path Posix (Rel GeneratedExternalCodeDir) Dir'

extImportToJsImport ::
  GeneratedCodeSourceDir d =>
  Path Posix (Rel d) (Dir GeneratedExternalCodeDir) ->
  Path Posix (Rel GeneratedExternalCodeDir) Dir' ->
  EI.ExtImport ->
  JsImport
extImportToJsImport pathToExternalCodeDir pathFromImportLocationToExtCodeDir extImport = makeJsImport importPath importName
  where
    userDefinedPath = SP.castRel $ EI.path extImport
    importName = extImportNameToJsImportName $ EI.name extImport
    importPath = SP.castRel pathFromImportLocationToExtCodeDir </> SP.castRel pathToExternalCodeDir </> userDefinedPath

    extImportNameToJsImportName :: EI.ExtImportName -> JsImportName
    extImportNameToJsImportName (EI.ExtImportModule name) = JsImportModule name
    extImportNameToJsImportName (EI.ExtImportField name) = JsImportField name
