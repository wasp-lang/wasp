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
import Wasp.JsImport (JsImport (..), JsImportName (JsImportField, JsImportModule), JsImportPath (..), getJsImportIdentifier, getJsImportPathString, getJsImportStmtAndIdentifier, getJsRuntimeDynamicImportExpression, getJsTypeDynamicImportExpression, makeJsImport)
import Wasp.Project.Common (srcDirInWaspProjectDir)

extImportToJsImport ::
  (GeneratedAppComponentSrcDir d) =>
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
jsImportToImportJson = maybe notDefinedValue mkTmplData
  where
    notDefinedValue :: Aeson.Value
    notDefinedValue = object ["isDefined" .= False]

    mkTmplData :: JsImport -> Aeson.Value
    mkTmplData jsImport =
      let (jsImportStatement, jsImportIdentifier) = getJsImportStmtAndIdentifier jsImport
       in object
            [ "isDefined" .= True,
              "importStatement" .= jsImportStatement,
              "importIdentifier" .= jsImportIdentifier,
              "runtimeDynamicImportExpression" .= getJsRuntimeDynamicImportExpression jsImport,
              "typeDynamicImportExpression" .= getJsTypeDynamicImportExpression jsImport,
              "importPath" .= getJsImportPathString jsImport,
              "exportName" .= getJsImportIdentifier jsImport
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
    { _path = ModuleImportPath virtualModuleId,
      _name = extImportNameToJsImportName extImport.name,
      _importAlias = Just $ getAliasedExtImportIdentifier extImport
    }

getAliasedExtImportIdentifier :: EI.ExtImport -> String
getAliasedExtImportIdentifier extImport = EI.importIdentifier extImport ++ "_ext"
