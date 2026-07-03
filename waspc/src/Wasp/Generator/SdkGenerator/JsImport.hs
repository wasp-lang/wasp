module Wasp.Generator.SdkGenerator.JsImport
  ( extImportToImportJson,
    VirtualModuleId,
  )
where

import qualified Data.Aeson as Aeson
import StrongPath (Dir', File', Path, Posix, Rel)
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.Generator.JsImport (getAliasedExtImportIdentifier)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.JsImport (JsImport (..), JsImportKind (ValueImport), JsImportPath (..))

type VirtualModuleId = Path Posix (Rel Dir') File'

-- | SDK is not allowed to import user exports (extImports) directly,
-- because that would cause a cyclic dependency. Instead, SDK resolves
-- user project imports through virtual modules.
--
-- For this to work properly, the virutal module also has to be registered
-- in the bundler of runtime that is going to use that part of code.
-- E.g. server bunlder has to register operations virtual modules.
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
