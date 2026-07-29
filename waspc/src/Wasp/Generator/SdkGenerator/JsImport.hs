module Wasp.Generator.SdkGenerator.JsImport
  ( extImportToImportJson,
  )
where

import qualified Data.Aeson as Aeson
import qualified Wasp.AppSpec.ExtImport as EI
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.SdkGenerator.VirtualUserModules (getVirtualUserModuleJsImportPath)
import Wasp.JsImport (JsImport (..), JsImportKind (ValueImport))

-- | Converts a 'EI.ExtImport' to virtual user module JS import JSON.
extImportToImportJson :: Maybe EI.ExtImport -> Aeson.Value
extImportToImportJson maybeExtImport = GJI.jsImportToImportJson jsImport
  where
    jsImport = extImportToVirtualUserModuleJsImport <$> maybeExtImport

-- | Converts a 'EI.ExtImport' to virtual user module JS import.
extImportToVirtualUserModuleJsImport ::
  EI.ExtImport ->
  JsImport
extImportToVirtualUserModuleJsImport extImport@(EI.ExtImport extImportName extImportPath _) =
  JsImport
    { _kind = ValueImport,
      _path = importPath,
      _name = importName,
      _importAlias = Just $ GJI.getAliasedExtImportIdentifier extImport
    }
  where
    importName = GJI.extImportNameToJsImportName extImportName
    importPath = getVirtualUserModuleJsImportPath extImportPath
