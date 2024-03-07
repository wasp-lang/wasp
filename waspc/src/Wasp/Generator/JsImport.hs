module Wasp.Generator.JsImport
  ( extImportToJsImport,
    jsImportToImportJson,
    extImportNameToJsImportName,
  )
where

import Data.Aeson (KeyValue ((.=)), object)
import qualified Data.Aeson as Aeson
import StrongPath (Dir, File', Path, Posix, Rel, (</>))
import qualified StrongPath as SP
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.Generator.Common (GeneratedSrcDir)
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.JsImport
  ( JsImport,
    JsImportName (JsImportField, JsImportModule),
    JsImportPath (RelativeImportPath),
    applyJsImportAlias,
    getJsImportStmtAndIdentifier,
    makeJsImport,
  )
import qualified Wasp.JsImport as JI
import Wasp.Util (toUpperFirst)

extImportToJsImport ::
  GeneratedSrcDir d =>
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
      let (jsImportStmt, jsImportIdentifier) = getJsImportStmtAndIdentifier $ mangleImportIdentifier jsImport
       in object
            [ "isDefined" .= True,
              "importStatement" .= jsImportStmt,
              "importIdentifier" .= jsImportIdentifier
            ]
      where
        mangleImportIdentifier :: JsImport -> JsImport
        mangleImportIdentifier JI.JsImport {JI._name = JsImportModule name} = prefixTheImportName name jsImport
        mangleImportIdentifier JI.JsImport {JI._name = JsImportField name} = prefixTheImportName name jsImport

        prefixTheImportName :: String -> JsImport -> JsImport
        prefixTheImportName originalName = applyJsImportAlias (Just ("__userDefined" ++ toUpperFirst originalName))
