module Wasp.Generator.JsImport
  ( extImportToJsImport,
    jsImportToImportJson,
    extImportToRelativeSrcImportFromViteExecution,
    getAliasedExtImportIdentifier,
    extImportNameToJsImportName,
  )
where

import Data.Aeson (KeyValue ((.=)), object)
import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import StrongPath (Dir, Path, Posix, Rel, (</>))
import qualified StrongPath as SP
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.AppSpec.ExternalFiles (SourceExternalCodeDir)
import Wasp.Generator.Common (GeneratedAppComponentSrcDir, dropExtensionFromImportPath)
import Wasp.JsImport
  ( JsImport (..),
    JsImportKind (..),
    JsImportName (JsImportField, JsImportModule),
    JsImportPath (RawImportName, RelativeImportPath),
    getJsDynamicImportExpression,
    getJsImportPathString,
    getJsImportStmtAndIdentifier,
    makeValueJsImport,
  )
import Wasp.Project.Common (srcDirInWaspProjectDir)

extImportToJsImport ::
  (GeneratedAppComponentSrcDir d) =>
  Path Posix (Rel d) (Dir SourceExternalCodeDir) ->
  Path Posix (Rel importLocation) (Dir d) ->
  EI.ExtImport ->
  JsImport
extImportToJsImport pathFromSrcDirToExtCodeDir pathFromImportLocationToSrcDir extImport = makeValueJsImport importPath importName
  where
    importName = extImportNameToJsImportName $ EI.name extImport
    importPath = case EI.source extImport of
      EI.ProjectSrcExtImportSource projectSrcPath ->
        RelativeImportPath $ SP.castRel $ pathFromImportLocationToSrcDir </> pathFromSrcDirToExtCodeDir </> projectSrcPath
      EI.PackageExtImportSource packageImportSource ->
        RawImportName $ EI.packageImportSourceToImportSpecifier packageImportSource

extImportNameToJsImportName :: EI.ExtImportName -> JsImportName
extImportNameToJsImportName (EI.ExtImportModule name) = JsImportModule name
extImportNameToJsImportName (EI.ExtImportField name) = JsImportField name

jsImportToImportJson :: Maybe JsImport -> Aeson.Value
jsImportToImportJson = maybe notDefinedImportJsonData mkImportJsonData
  where
    notDefinedImportJsonData :: Aeson.Value
    notDefinedImportJsonData = object ["isDefined" .= False]

    mkImportJsonData :: JsImport -> Aeson.Value
    mkImportJsonData jsImport =
      object
        [ "isDefined" .= True,
          "importPath" .= getJsImportPathString jsImport,
          "importIdentifier" .= jsImportIdentifier,
          "importStatement" .= jsImportStatement,
          "dynamicImportExpression" .= getJsDynamicImportExpression jsImport
        ]
      where
        (jsImportStatement, jsImportIdentifier) = getJsImportStmtAndIdentifier jsImport

extImportToRelativeSrcImportFromViteExecution :: EI.ExtImport -> JsImport
extImportToRelativeSrcImportFromViteExecution extImport@(EI.ExtImport extImportName extImportSource _) =
  JsImport
    { _kind = ValueImport,
      _path = importPath,
      _name = importName,
      _importAlias = Just $ getAliasedExtImportIdentifier extImport
    }
  where
    importName = extImportNameToJsImportName extImportName
    importPath = case extImportSource of
      EI.ProjectSrcExtImportSource projectSrcPath ->
        RelativeImportPath $ SP.castRel $ dropExtensionFromImportPath $ projectSrcDir </> projectSrcPath
      EI.PackageExtImportSource packageImportSource ->
        RawImportName $ EI.packageImportSourceToImportSpecifier packageImportSource
    projectSrcDir = fromJust (SP.relDirToPosix srcDirInWaspProjectDir)

extImportNameToJsImportName :: EI.ExtImportName -> JsImportName
extImportNameToJsImportName (EI.ExtImportModule name) = JsImportModule name
extImportNameToJsImportName (EI.ExtImportField name) = JsImportField name

getAliasedExtImportIdentifier :: EI.ExtImport -> String
getAliasedExtImportIdentifier extImport = EI.importIdentifier extImport ++ "_ext"
