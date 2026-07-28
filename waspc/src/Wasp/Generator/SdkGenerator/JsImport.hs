module Wasp.Generator.SdkGenerator.JsImport
  ( extImportToImportJson,
  )
where

import qualified Data.Aeson as Aeson
import qualified Wasp.AppSpec.ExtImport as EI
import qualified Wasp.Generator.JsImport as GJI
import Wasp.JsImport (JsImport (..), JsImportKind (ValueImport))

-- | The SDK must not import values from the user project (ext imports) directly,
-- because that would create a cyclic dependency between the TypeScript projects.
-- TypeScript can't compile projects that have cyclic dependencies.
-- So we must find a way to use the user project values without the SDK depending
-- on the user project.
--
-- Copying the user project and making the SDK depend on the copy is not an option,
-- because it forces the user's project to compile with the SDK's TypeScript config.
-- ({@link https://github.com/wasp-lang/wasp/issues/2247 Old issue about the problem})
--
-- The direct way to do that would be to use DI (dependency injection).
-- However, DI systems are fragile to module initialization order; we must push
-- the user project values before the SDK tries to use them.
--
-- Instead, we will proxy the user project imports through virtual modules.
-- Virtual modules are resolved bundle time, ....
--
-- What we esentially do, is to delay SDK's user project imports resolution
-- to bundle time which happens after the TypeScript projects have been compiled.
--
-- The generates templates code is almost identical to direct imports.
-- The only difference is that we import from a virtual module:
-- ```ts title="./sdk/wasp/client/env/schema.ts"
-- import { clientEnvValidationSchema as clientEnvValidationSchema_ext } from "virtual:wasp/user/env";
-- const userClientEnvSchema: UserClientEnvSchema = clientEnvValidationSchema_ext;
-- ```
--
-- For virtual modules to be resolved properly, each runtime that uses the SDK
-- (the server and the client) must equip its bundler with a plugin that resolves
-- the virtual user modules the SDK references into actual user files.
-- E.g., client virtual user modules plugin:
-- ```ts title="./sdk/wasp/client/vite/plugins/waspVirtualUserModules.ts"
-- const clientVirtualUserModuleMap: { [virtualUserModule: string]: string } = {
--   'virtual:wasp/user/env': './src/env',
-- };
--
-- export function waspVirtualUserModules(): Plugin {
--    // ...
--     async resolveId(id, importer, options) {
--       if (id in clientVirtualUserModuleMap) {
--         const absPath = path.resolve(clientRootDir, clientVirtualUserModuleMap[id]);
--         return this.resolve(absPath, importer, { ...options, skipSelf: true });
--       }
--       return null;
--     },
--   };
-- }
-- ```ts
--
-- While this makes everything work in the runtime, it would fail TypeScript
-- compilation because virtual modules are still `undefined`.
-- So the last thing to do is module declaration of the virtual user modules
-- used by the SDK:
-- ```ts title="./sdk/wasp/wasp-user-virtual-modules.d.ts"
-- declare module "virtual:wasp/user/env" {
--   import type { RegisteredClientEnvValidationSchema } from "./client/env/schema";
--
--   export const clientEnvValidationSchema: RegisteredClientEnvValidationSchema;
-- }
-- ```
extImportToImportJson :: Maybe EI.ExtImport -> Aeson.Value
extImportToImportJson maybeExtImport = GJI.jsImportToImportJson jsImport
  where
    jsImport = extImportToVirtualUserModuleJsImport <$> maybeExtImport

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
    importPath = GJI.getVirtualUserModuleJsImportPath extImportPath
