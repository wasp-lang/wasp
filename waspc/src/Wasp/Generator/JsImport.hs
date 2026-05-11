{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Generator.JsImport
  ( extImportToJsImport,
    jsImportToImportJson,
    extImportNameToJsImportName,
    getAliasedExtImportIdentifier,
    extImportToRelativeSrcImportFromViteExecution,
    virtualExtImportToImportJson,
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
import Wasp.Generator.UserVirtualModules (VirtualModuleId)
import Wasp.JsImport (JsImport (..), JsImportKind (..), JsImportName (JsImportField, JsImportModule), JsImportPath (..), getJsDynamicImportExpression, getJsImportIdentifier, getJsImportPathString, getJsImportStmtAndIdentifier, makeValueJsImport)
import Wasp.Project.Common (srcDirInWaspProjectDir)

extImportToJsImport ::
  (GeneratedAppComponentSrcDir d) =>
  Path Posix (Rel d) (Dir GeneratedExternalCodeDir) ->
  Path Posix (Rel importLocation) (Dir d) ->
  EI.ExtImport ->
  JsImport
extImportToJsImport pathFromSrcDirToExtCodeDir pathFromImportLocationToSrcDir extImport = makeValueJsImport (RelativeImportPath importPath) importName
  where
    importName = extImportNameToJsImportName $ EI.name extImport
    importPath = SP.castRel $ pathFromImportLocationToSrcDir </> pathFromSrcDirToExtCodeDir </> userDefinedPathInExtSrcDir
    userDefinedPathInExtSrcDir = SP.castRel $ EI.path extImport :: Path Posix (Rel GeneratedExternalCodeDir) File'

extImportNameToJsImportName :: EI.ExtImportName -> JsImportName
extImportNameToJsImportName (EI.ExtImportModule name) = JsImportModule name
extImportNameToJsImportName (EI.ExtImportField name) = JsImportField name

jsImportToImportJson :: Maybe JsImport -> Aeson.Value
jsImportToImportJson = maybe notDefinedImportJsonData mkTmplData
  where
    notDefinedImportJsonData :: Aeson.Value
    notDefinedImportJsonData = object ["isDefined" .= False]

    mkTmplData :: JsImport -> Aeson.Value
    mkTmplData jsImport =
      object
        [ "isDefined" .= True,
          "importPath" .= getJsImportPathString jsImport,
          "exportName" .= getJsImportIdentifier jsImport,
          "importIdentifier" .= jsImportIdentifier,
          "importStatement" .= jsImportStatement,
          "dynamicImportExpression" .= getJsDynamicImportExpression jsImport
        ]
      where
        (jsImportStatement, jsImportIdentifier) = getJsImportStmtAndIdentifier jsImport

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

virtualExtImportToImportJson :: VirtualModuleId -> Maybe EI.ExtImport -> Aeson.Value
virtualExtImportToImportJson virtualModuleId maybeExtImport =
  jsImportToImportJson jsImport
  where
    jsImport = virtualExtImportToJsImport virtualModuleId <$> maybeExtImport

-- Creates a JS import for an external file which is resolved
-- through a virtual module.
virtualExtImportToJsImport :: VirtualModuleId -> EI.ExtImport -> JsImport
virtualExtImportToJsImport virtualModuleId extImport =
  JsImport
    { _kind = ValueImport,
      _path = ModuleImportPath virtualModuleId,
      _name = extImportNameToJsImportName extImport.name,
      _importAlias = Just $ getAliasedExtImportIdentifier extImport
    }

getAliasedExtImportIdentifier :: EI.ExtImport -> String
getAliasedExtImportIdentifier extImport = EI.importIdentifier extImport ++ "_ext"
