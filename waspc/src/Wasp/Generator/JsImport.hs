module Wasp.Generator.JsImport
  ( extImportToJsImport,
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
    makeJsImport,
  )

type RelDirToExternalCodeDir = Path Posix (Rel GeneratedExternalCodeDir) Dir'

extImportToJsImport ::
  -- | Path to directory with external code, relative to generated code source dir (server or web app).
  Path Posix (Rel ()) (Dir GeneratedExternalCodeDir) ->
  RelDirToExternalCodeDir ->
  EI.ExtImport ->
  JsImport
extImportToJsImport pathToExternalCodeDir relDirToExternalCodeDir extImport = makeJsImport importPath importName
  where
    userDefinedPath = SP.castRel $ EI.path extImport
    importName = extImportNameToJsImportName $ EI.name extImport
    importPath = SP.castRel $ relDirToExternalCodeDir </> pathToExternalCodeDir </> userDefinedPath

extImportNameToJsImportName :: EI.ExtImportName -> JsImportName
extImportNameToJsImportName (EI.ExtImportModule name) = JsImportModule name
extImportNameToJsImportName (EI.ExtImportField name) = JsImportField name
