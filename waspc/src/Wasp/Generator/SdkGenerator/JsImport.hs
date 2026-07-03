module Wasp.Generator.SdkGenerator.JsImport
  ( extImportToImportJson,
  )
where

import qualified Data.Aeson as Aeson
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.Generator.JsImport (getAliasedExtImportIdentifier)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.UserVirtualModules (VirtualModuleId)
import Wasp.JsImport (JsImport (..), JsImportKind (ValueImport), JsImportPath (..))

-- | SDK is not allowed to import user code (extImports) directly,
-- because that would cause a TypeScript project cyclic dependency.
-- Instead, SDK resolves user code imports through virtual modules.
--
-- For this to work properly, the virutal module must be resolveable by
-- the bundler of runtime that is going to use that user code.
-- E.g., server bunlder has to know how to resolve operations virtual modules.
extImportToImportJson :: VirtualModuleId -> Maybe EI.ExtImport -> Aeson.Value
extImportToImportJson virtualModuleId maybeExtImport = GJI.jsImportToImportJson jsImport
  where
    jsImport = extImportToJsImport virtualModuleId <$> maybeExtImport

extImportToJsImport :: VirtualModuleId -> EI.ExtImport -> JsImport
extImportToJsImport virtualModuleId extImport@(EI.ExtImport extImportName _ _) =
  JsImport
    { _kind = ValueImport,
      _path = ModuleImportPath virtualModuleId,
      _name = importName,
      _importAlias = Just $ getAliasedExtImportIdentifier extImport
    }
  where
    importName = GJI.extImportNameToJsImportName extImportName
