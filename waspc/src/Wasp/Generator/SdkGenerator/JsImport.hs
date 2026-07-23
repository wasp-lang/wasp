module Wasp.Generator.SdkGenerator.JsImport
  ( extImportToImportJson,
  )
where

import qualified Data.Aeson as Aeson
import qualified Wasp.AppSpec.ExtImport as EI
import qualified Wasp.Generator.JsImport as GJI

-- | SDK is not allowed to import from user project (ext imports) directly,
-- because that would create a cyclic dependency between TypeScript projects.
-- Instead, SDK resolves user project imports through virtual modules.
extImportToImportJson :: Maybe EI.ExtImport -> Aeson.Value
extImportToImportJson maybeExtImport = GJI.jsImportToImportJson jsImport
  where
    jsImport = GJI.extImportToVirtualUserModuleJsImport <$> maybeExtImport
