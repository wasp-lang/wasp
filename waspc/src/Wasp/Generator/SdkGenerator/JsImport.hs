module Wasp.Generator.SdkGenerator.JsImport
  ( extImportToImportJson,
  )
where

import qualified Data.Aeson as Aeson
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.Generator.JsImport (getAliasedExtImportIdentifier)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.UserVirtualModules (UserVirtualModuleId)
import Wasp.JsImport (JsImport (..), JsImportKind (ValueImport), JsImportPath (..))

-- | SDK is not allowed to import from user project (ext imports) directly,
-- because that would create a cyclic dependency between TypeScript projects.
-- Instead, SDK resolves user project imports through virtual modules.
extImportToImportJson :: UserVirtualModuleId -> Maybe EI.ExtImport -> Aeson.Value
extImportToImportJson virtualModuleId maybeExtImport = GJI.jsImportToImportJson jsImport
  where
    jsImport = extImportToJsImport virtualModuleId <$> maybeExtImport

extImportToJsImport :: UserVirtualModuleId -> EI.ExtImport -> JsImport
extImportToJsImport virtualModuleId extImport@(EI.ExtImport extImportName _ _) =
  JsImport
    { _kind = ValueImport,
      -- Import from virtual module instead.
      _path = ModuleImportPath virtualModuleId,
      _name = GJI.extImportNameToJsImportName extImportName,
      _importAlias = Just $ getAliasedExtImportIdentifier extImport
    }
