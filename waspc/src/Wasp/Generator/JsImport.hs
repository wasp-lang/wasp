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
import Wasp.Generator.Common (GeneratedAppComponentSrcDir, dropExtensionFromImportPath)
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.JsImport
  ( JsImport (..),
    JsImportKind (..),
    JsImportName (JsImportField, JsImportModule),
    JsImportPath (RawImportName, RelativeImportPath),
    getJsDynamicImportExpression,
    getJsImportStmtAndIdentifier,
    makeValueJsImport,
  )
import Wasp.Project.Common (srcDirInWaspProjectDir)

extImportToJsImport ::
  (GeneratedAppComponentSrcDir d) =>
  Path Posix (Rel d) (Dir GeneratedExternalCodeDir) ->
  Path Posix (Rel importLocation) (Dir d) ->
  EI.ExtImport ->
  JsImport
extImportToJsImport pathFromSrcDirToExtCodeDir pathFromImportLocationToSrcDir extImport = makeValueJsImport importPath importName
  where
    importName = extImportNameToJsImportName $ EI.name extImport
    importPath = case EI.source extImport of
      EI.ProjectSrcExtImportSource projectSrcPath ->
        RelativeImportPath $ SP.castRel $ pathFromImportLocationToSrcDir </> pathFromSrcDirToExtCodeDir </> toGeneratedExtCodePath projectSrcPath
      EI.PackageExtImportSource packageImportSource ->
        RawImportName $ EI.packageImportSourceToImportSpecifier packageImportSource
    toGeneratedExtCodePath projectSrcPath = SP.castRel projectSrcPath :: Path Posix (Rel GeneratedExternalCodeDir) File'

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
      let (jsImportStmt, jsImportIdentifier) = getJsImportStmtAndIdentifier jsImport
       in object
            [ "isDefined" .= True,
              "importStatement" .= jsImportStmt,
              "importIdentifier" .= jsImportIdentifier,
              "dynamicImportExpression" .= getJsDynamicImportExpression jsImport
            ]

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

getAliasedExtImportIdentifier :: EI.ExtImport -> String
getAliasedExtImportIdentifier extImport = EI.importIdentifier extImport ++ "_ext"
