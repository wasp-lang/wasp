module Wasp.Generator.JsImport
  ( mkJsImportStmtAndIdentifierGetterFromExtSrcDir,
    RelDirToExternalCodeDir,
  )
where

import StrongPath (Dir, Dir', Path, Posix, Rel, (</>))
import qualified StrongPath as SP
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.JsImport
  ( JsImportAlias,
    JsImportIdentifier,
    JsImportName (JsImportField, JsImportModule),
    JsImportStatement,
    getJsImportStmtAndIdentifier,
  )

type RelDirToExternalCodeDir = Path Posix (Rel GeneratedExternalCodeDir) Dir'

mkJsImportStmtAndIdentifierGetterFromExtSrcDir ::
  Path Posix (Rel ()) (Dir GeneratedExternalCodeDir) ->
  ( RelDirToExternalCodeDir ->
    EI.ExtImport ->
    Maybe JsImportAlias ->
    (JsImportStatement, JsImportIdentifier)
  )
mkJsImportStmtAndIdentifierGetterFromExtSrcDir rootDir relDirToExternalCodeDir extImport importAlias = getJsImportStmtAndIdentifier importPath importName importAlias
  where
    userDefinedPath = SP.castRel $ EI.path extImport
    importName = extImportNameToJsImportName $ EI.name extImport
    importPath = SP.castRel $ relDirToExternalCodeDir </> rootDir </> userDefinedPath

extImportNameToJsImportName :: EI.ExtImportName -> JsImportName
extImportNameToJsImportName (EI.ExtImportModule name) = JsImportModule name
extImportNameToJsImportName (EI.ExtImportField name) = JsImportField name
