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
    JsImportPath (RelativeImportPath),
    getJsDynamicImportExpression,
    getJsImportStmtAndIdentifier,
    makeTypeJsImport,
    makeValueJsImport,
  )
import Wasp.Project.Common (srcDirInWaspProjectDir)

extImportToJsImport ::
  (GeneratedAppComponentSrcDir d) =>
  Path Posix (Rel d) (Dir GeneratedExternalCodeDir) ->
  Path Posix (Rel importLocation) (Dir d) ->
  EI.ExtImport ->
  JsImport
extImportToJsImport pathFromSrcDirToExtCodeDir pathFromImportLocationToSrcDir extImport =
  if isValueImport
    then makeValueJsImport (RelativeImportPath importPath) importName
    else makeTypeJsImport (RelativeImportPath importPath) importName
  where
    -- For now treat all external imports as value imports.
    -- If external type imports are necessary in future,
    -- add 'importKind' to 'ExtImport' and branch on it.
    isValueImport = True
    importName = extImportNameToJsImportName $ EI.name extImport
    importPath = SP.castRel $ pathFromImportLocationToSrcDir </> pathFromSrcDirToExtCodeDir </> userDefinedPathInExtSrcDir
    userDefinedPathInExtSrcDir = SP.castRel $ EI.path extImport :: Path Posix (Rel GeneratedExternalCodeDir) File'

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
extImportToRelativeSrcImportFromViteExecution extImport@(EI.ExtImport extImportName extImportPath) =
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

getAliasedExtImportIdentifier :: EI.ExtImport -> String
getAliasedExtImportIdentifier extImport = EI.importIdentifier extImport ++ "_ext"
