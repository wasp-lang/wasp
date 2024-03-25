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
import Wasp.Generator.Monad (Generator, mangleName)
import Wasp.JsImport
  ( JsImport,
    JsImportName (JsImportField, JsImportModule),
    JsImportPath (RelativeImportPath),
    applyJsImportAlias,
    getJsImportStmtAndIdentifier,
    makeJsImport,
  )
import qualified Wasp.JsImport as JI

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

jsImportToImportJson :: Maybe JsImport -> Generator Aeson.Value
jsImportToImportJson maybeJsImport = maybe notDefinedValue mkTmplData maybeJsImport
  where
    notDefinedValue = return $ object ["isDefined" .= False]

    mkTmplData :: JsImport -> Generator Aeson.Value
    mkTmplData jsImport = do
      mangledJsImport <- mangleImportIdentifier jsImport
      let (jsImportStmt, jsImportIdentifier) = getJsImportStmtAndIdentifier mangledJsImport
      return $
        object
          [ "isDefined" .= True,
            "importStatement" .= jsImportStmt,
            "importIdentifier" .= jsImportIdentifier
          ]
      where
        mangleImportIdentifier :: JsImport -> Generator JsImport
        mangleImportIdentifier JI.JsImport {JI._name = JsImportModule name} = mangleJsImportName name jsImport
        mangleImportIdentifier JI.JsImport {JI._name = JsImportField name} = mangleJsImportName name jsImport

        mangleJsImportName :: String -> JsImport -> Generator JsImport
        mangleJsImportName originalName originalJsImport = do
          mangledName <- mangleName originalName

          return $ applyJsImportAlias (Just mangledName) originalJsImport
