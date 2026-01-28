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
    JsImportPath (RelativeImportPath),
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
extImportToJsImport pathFromSrcDirToExtCodeDir pathFromImportLocationToSrcDir extImport = makeJsImport (RelativeImportPath importPath) importName
  where
    userDefinedPathInExtSrcDir = SP.castRel $ EI.path extImport :: Path Posix (Rel GeneratedExternalCodeDir) File'
    importName = extImportNameToJsImportName $ EI.name extImport
    importPath = SP.castRel $ pathFromImportLocationToSrcDir </> pathFromSrcDirToExtCodeDir </> userDefinedPathInExtSrcDir

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
extImportToRelativeSrcImportFromViteExecution extImport@(EI.ExtImport extImportName extImportPath) =
  JsImport
    { _path = RelativeImportPath relativePath,
      _name = importName,
      _importAlias = Just $ getAliasedExtImportIdentifier extImport
    }
  where
    relativePath = SP.castRel $ dropExtensionFromImportPath $ projectSrcDir </> extImportPath
    projectSrcDir = fromJust (SP.relDirToPosix srcDirInWaspProjectDir)
    importName = extImportNameToJsImportName extImportName

getAliasedExtImportIdentifier :: EI.ExtImport -> String
getAliasedExtImportIdentifier extImport = EI.importIdentifier extImport ++ "_ext"
