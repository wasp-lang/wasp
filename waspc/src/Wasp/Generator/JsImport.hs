module Wasp.Generator.JsImport
  ( extImportToJsImport,
    jsImportToImportJson,
    extImportNameToJsImportName,
    extImportSourceToJsImportPath,
    getAliasedExtImportIdentifier,
    extImportToJsImportFromViteExecution,
  )
where

import Data.Aeson (KeyValue ((.=)), object)
import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import StrongPath (Dir, Path, Posix, Rel, (</>))
import qualified StrongPath as SP
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.AppSpec.ExtImport.Source (ExtImportSource, ProjectSrcExtImportPath)
import qualified Wasp.AppSpec.ExtImport.Source as ExtImportSource
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
    importPath = extImportSourceToJsImportPath projectSrcPathToJsImportPath $ EI.source extImport
    projectSrcPathToJsImportPath projectSrcPath =
      RelativeImportPath $ SP.castRel $ pathFromImportLocationToSrcDir </> pathFromSrcDirToExtCodeDir </> projectSrcPath

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

extImportToJsImportFromViteExecution :: EI.ExtImport -> JsImport
extImportToJsImportFromViteExecution extImport@(EI.ExtImport extImportName extImportSource _) =
  JsImport
    { _kind = ValueImport,
      _path = importPath,
      _name = importName,
      _importAlias = Just $ getAliasedExtImportIdentifier extImport
    }
  where
    importName = extImportNameToJsImportName extImportName
    importPath = extImportSourceToJsImportPath projectSrcPathToJsImportPath extImportSource
    projectSrcPathToJsImportPath projectSrcPath =
      RelativeImportPath $ SP.castRel $ dropExtensionFromImportPath $ projectSrcDir </> projectSrcPath
    projectSrcDir = fromJust (SP.relDirToPosix srcDirInWaspProjectDir)

extImportSourceToJsImportPath ::
  (ProjectSrcExtImportPath -> JsImportPath) ->
  ExtImportSource ->
  JsImportPath
extImportSourceToJsImportPath projectSrcPathToJsImportPath extImportSource = case extImportSource of
  ExtImportSource.ProjectSrcExtImportSource projectSrcPath -> projectSrcPathToJsImportPath projectSrcPath
  ExtImportSource.PackageExtImportSource packageImportSource ->
    RawImportName $ ExtImportSource.packageImportSourceToImportSpecifier packageImportSource

getAliasedExtImportIdentifier :: EI.ExtImport -> String
getAliasedExtImportIdentifier extImport = EI.importIdentifier extImport ++ "_ext"
