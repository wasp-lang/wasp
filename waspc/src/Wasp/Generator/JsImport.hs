module Wasp.Generator.JsImport
  ( extImportToJsImport,
    jsImportToImportJson,
    extImportNameToJsImportName,
    getAliasedExtImportIdentifier,
    extImportToRelativeSrcImportFromViteExecution,
  )
where

import Data.Aeson (KeyValue ((.=)), object)
import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import StrongPath (Dir, File', Path, Posix, Rel, (</>))
import qualified StrongPath as SP
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.Generator.Common (GeneratedSrcDir, dropExtensionFromImportPath)
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.JsImport
  ( JsImport (..),
    JsImportName (JsImportField, JsImportModule),
    JsImportPath (RawModuleImportPath, RelativeImportPath),
    getJsImportStmtAndIdentifier,
    makeJsImport,
  )
import Wasp.Project.Common (srcDirInWaspProjectDir)

extImportToJsImport ::
  (GeneratedSrcDir d) =>
  Path Posix (Rel d) (Dir GeneratedExternalCodeDir) ->
  Path Posix (Rel importLocation) (Dir d) ->
  EI.ExtImport ->
  JsImport
extImportToJsImport pathFromSrcDirToExtCodeDir pathFromImportLocationToSrcDir extImport =
  case EI.path extImport of
    EI.ExtImportSrcPath srcPath ->
      makeJsImport (RelativeImportPath importPath) importName
      where
        userDefinedPathInExtSrcDir = SP.castRel srcPath :: Path Posix (Rel GeneratedExternalCodeDir) File'
        importPath = SP.castRel $ pathFromImportLocationToSrcDir </> pathFromSrcDirToExtCodeDir </> userDefinedPathInExtSrcDir
    EI.ExtImportPkgPath pkgPath ->
      makeJsImport (RawModuleImportPath pkgPath) importName
  where
    importName = extImportNameToJsImportName $ EI.name extImport

extImportNameToJsImportName :: EI.ExtImportName -> JsImportName
extImportNameToJsImportName (EI.ExtImportModule name) = JsImportModule name
extImportNameToJsImportName (EI.ExtImportField name) = JsImportField name

jsImportToImportJson :: Maybe JsImport -> Aeson.Value
jsImportToImportJson maybeJsImport = maybe notDefinedValue mkTmplData maybeJsImport
  where
    notDefinedValue = object ["isDefined" .= False]

    mkTmplData :: JsImport -> Aeson.Value
    mkTmplData jsImport =
      let (jsImportStmt, jsImportIdentifier) = getJsImportStmtAndIdentifier jsImport
       in object
            [ "isDefined" .= True,
              "importStatement" .= jsImportStmt,
              "importIdentifier" .= jsImportIdentifier
            ]

extImportToRelativeSrcImportFromViteExecution :: EI.ExtImport -> JsImport
extImportToRelativeSrcImportFromViteExecution extImport =
  case EI.path extImport of
    EI.ExtImportSrcPath srcPath ->
      JsImport
        { _path = RelativeImportPath relativePath,
          _name = importName,
          _importAlias = Just $ getAliasedExtImportIdentifier extImport
        }
      where
        relativePath = SP.castRel $ dropExtensionFromImportPath $ projectSrcDir </> srcPath
        projectSrcDir = fromJust (SP.relDirToPosix srcDirInWaspProjectDir)
    EI.ExtImportPkgPath pkgPath ->
      JsImport
        { _path = RawModuleImportPath pkgPath,
          _name = importName,
          _importAlias = Just $ getAliasedExtImportIdentifier extImport
        }
  where
    importName = extImportNameToJsImportName $ EI.name extImport

getAliasedExtImportIdentifier :: EI.ExtImport -> String
getAliasedExtImportIdentifier extImport = EI.importIdentifier extImport ++ "_ext"
