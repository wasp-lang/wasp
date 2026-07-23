module Wasp.Generator.SdkGenerator.JsImport
  ( extImportToImportJson,
  )
where

import qualified Data.Aeson as Aeson
import qualified Wasp.AppSpec.ExtImport as EI
import qualified Wasp.Generator.JsImport as GJI

-- | The SDK must not import values from the user project (ext imports)
-- directly, because that would create a cyclic dependency between the
-- TypeScript projects. Instead, the SDK imports user code through virtual
-- modules.
--
-- Virtual modules are resolved bundle time, so each runtime that uses the
-- SDK (server and client) must equip its bundler with a plugin that resolves
-- the virtual user modules the SDK references into actual user files.
--
-- For example, the SDK imports the user's operation definitions through
-- virtual modules. Operations execute in the server runtime, so it's the
-- server's bundler that resolves those virtual modules to the user's files.
extImportToImportJson :: Maybe EI.ExtImport -> Aeson.Value
extImportToImportJson maybeExtImport = GJI.jsImportToImportJson jsImport
  where
    jsImport = GJI.extImportToVirtualUserModuleJsImport <$> maybeExtImport
