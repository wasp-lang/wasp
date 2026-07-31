module Wasp.Generator.SdkGenerator.JsImport
  ( extImportToImportJson,
    extImportToJsImport,
  )
where

import qualified Data.Aeson as Aeson
import qualified Wasp.AppSpec.ExtImport as EI
import qualified Wasp.Generator.JsImport as GJI
import qualified Wasp.Generator.SdkGenerator.VirtualUserModules as VUM
import Wasp.JsImport (JsImport (..), JsImportKind (ValueImport))

extImportToImportJson :: Maybe EI.ExtImport -> Aeson.Value
extImportToImportJson maybeExtImport = GJI.jsImportToImportJson jsImport
  where
    jsImport = extImportToJsImport <$> maybeExtImport

extImportToJsImport ::
  EI.ExtImport ->
  JsImport
extImportToJsImport extImport@(EI.ExtImport extImportName extImportSource _) =
  JsImport
    { _kind = ValueImport,
      _path = importPath,
      _name = importName,
      _importAlias = Just $ GJI.getAliasedExtImportIdentifier extImport
    }
  where
    importPath = GJI.extImportSourceToJsImportPath projectSrcPathToJsImportPath extImportSource
    projectSrcPathToJsImportPath = VUM.extImportToVirtualUserModuleJsImportPath
    importName = GJI.extImportNameToJsImportName extImportName
