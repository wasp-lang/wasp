module Wasp.Generator.JsImport
  ( extImportToJsImport,
    jsImportToImportJson,
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
    getJsImportStmtAndIdentifier,
    makeJsImport,
  )

extImportToJsImport ::
  GeneratedSrcDir d =>
  Path Posix (Rel d) (Dir GeneratedExternalCodeDir) ->
  Path Posix (Rel importLocation) (Dir d) ->
  EI.ExtImport ->
  JsImport
extImportToJsImport pathFromSrcDirToExtCodeDir pathFromImportLocationToSrcDir extImport = makeJsImport importPath importName
  where
    userDefinedPathInExtSrcDir = SP.castRel $ EI.path extImport :: Path Posix (Rel GeneratedExternalCodeDir) File'
    importName = extImportNameToJsImportName $ EI.name extImport
    importPath = SP.castRel $ pathFromImportLocationToSrcDir </> pathFromSrcDirToExtCodeDir </> userDefinedPathInExtSrcDir

    extImportNameToJsImportName :: EI.ExtImportName -> JsImportName
    extImportNameToJsImportName (EI.ExtImportModule name) = JsImportModule name
    extImportNameToJsImportName (EI.ExtImportField name) = JsImportField name

-- filip: attempt to simplify how we generate imports. I wanted to generate a
-- module import (e.g., '@ext-src/something') and couldn't do it
-- jsImportToImportJsonRaw :: Maybe (FilePath, JsImportName, Maybe JsImportAlias) -> Aeson.Value
-- jsImportToImportJsonRaw importData = maybe notDefinedValue mkTmplData importData
--   where
--     notDefinedValue = object ["isDefined" .= False]

--     mkTmplData :: (FilePath, JsImportName, Maybe JsImportAlias) -> Aeson.Value
--     mkTmplData (importPath, importName, maybeImportAlias) =
--       let (jsImportStmt, jsImportIdentifier) = getJsImportStmtAndIdentifierRaw importPath importName maybeImportAlias
--        in object
--             [ "isDefined" .= True,
--               "importStatement" .= jsImportStmt,
--               "importIdentifier" .= jsImportIdentifier
--             ]

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
