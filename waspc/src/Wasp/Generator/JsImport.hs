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
    JsImportPath (RelativeImportPath),
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
extImportToJsImport pathFromSrcDirToExtCodeDir pathFromImportLocationToSrcDir (EI.ExtImport extImportName extImportPath _) =
  makeValueJsImport (RelativeImportPath importPath) importName
  where
    importName = extImportNameToJsImportName extImportName
    importPath = SP.castRel $ pathFromImportLocationToSrcDir </> pathFromSrcDirToExtCodeDir </> extImportPath

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
extImportToRelativeSrcImportFromViteExecution extImport@(EI.ExtImport extImportName extImportPath _) =
  JsImport
    { _kind = ValueImport,
      _path = RelativeImportPath importPath,
      _name = importName,
      _importAlias = Just $ getAliasedExtImportIdentifier extImport
    }
  where
    importName = extImportNameToJsImportName extImportName
    importPath = SP.castRel $ dropExtensionFromImportPath $ projectSrcDir </> extImportPath
    projectSrcDir = fromJust (SP.relDirToPosix srcDirInWaspProjectDir)

extImportNameToJsImportName :: EI.ExtImportName -> JsImportName
extImportNameToJsImportName (EI.ExtImportModule name) = JsImportModule name
extImportNameToJsImportName (EI.ExtImportField name) = JsImportField name

getAliasedExtImportIdentifier :: EI.ExtImport -> String
getAliasedExtImportIdentifier extImport = EI.importIdentifier extImport ++ "_ext"
