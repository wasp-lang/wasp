module Wasp.Generator.SdkGenerator.JsImport
  ( extImportToImportJson,
  )
where

import qualified Data.Aeson as Aeson
import qualified Wasp.AppSpec.ExtImport as EI
import qualified Wasp.Generator.JsImport as GJI
import Wasp.JsImport (JsImport (..), JsImportKind (ValueImport))

-- | This function allows the SDK to use the user project values (ext imports).
--
-- If the SDK tried to import values from the user project directly,
-- it would create a cyclic dependency between TypeScript projects.
-- TypeScript can't compile projects that have cyclic dependencies.
-- So we must find a way to use the user project values without the
-- SDK depending on the user project.
--
-- Copying the user project and making the SDK depend on the copy
-- is not an option, because it forces the user's project to compile
-- with the SDK's TypeScript config.
-- (Old issue about the problem: https://github.com/wasp-lang/wasp/issues/2247)
--
-- The direct way to do that would be DI (dependency injection).
-- However, DI is fragile to module initialization order (values must be
-- registered before they can be used), so we want to avoid that approach.
--
-- Instead, the solution is to proxy the user project imports through
-- virtual modules. Bundler resolves virtual user modules at bundle time,
-- which happens after all TypeScript projects have been compiled.
-- This delays the resolution of the SDK's user project imports until
-- after compilation, so the compiler never sees a cycle.
--
-- Virtual user module imports look almost identical to direct imports,
-- just the module is a virtual user module instead of a relative path:
--
-- > // ./sdk/wasp/client/env/schema.ts
-- > export type RegisteredClientEnvValidationSchema = FromRegister<"clientEnvValidationSchema", z.ZodObject<{}>>;
-- > type UserClientEnvSchema = RegisteredClientEnvValidationSchema;
-- >
-- > import { clientEnvValidationSchema as clientEnvValidationSchema_ext } from "virtual:wasp/user/env";
-- > const userClientEnvSchema: UserClientEnvSchema = clientEnvValidationSchema_ext;
--
-- For virtual modules to be resolved properly, each runtime that uses
-- the SDK (the server and the client) must equip its bundler with a
-- plugin that resolves virtual user modules into actual user files.
--
-- Each plugin resolves only the virtual user modules that end up in its
-- runtime's bundle. E.g., the server plugin resolves server-side user modules
-- like operations, but not the client env validation schema.
-- E.g., the client plugin:
--
-- > // ./sdk/wasp/client/vite/plugins/waspVirtualUserModules.ts
-- > const clientVirtualUserModuleMap: { [virtualUserModule: string]: string } = {
-- >   'virtual:wasp/user/env': './src/env',
-- > };
-- >
-- > export function waspVirtualUserModules(): Plugin {
-- >   // ...
-- >   async resolveId(id, importer, options) {
-- >     if (Object.hasOwn(clientVirtualUserModuleMap, id)) {
-- >       const absPath = path.resolve(clientRootDir, clientVirtualUserModuleMap[id]);
-- >       return this.resolve(absPath, importer, { ...options, skipSelf: true });
-- >     }
-- >     return null;
-- >   },
-- > }
--
-- While this makes everything work at runtime, TypeScript compilation
-- would still fail because the virtual modules don't exist at compile
-- time. The last step is a module declaration for every virtual user
-- module the SDK uses:
--
-- > // ./sdk/wasp/wasp-user-virtual-modules.d.ts
-- > declare module "virtual:wasp/user/env" {
-- >   export const clientEnvValidationSchema: import("./client/env/schema").RegisteredClientEnvValidationSchema;
-- > }
-- > // Many other declarations....
--
-- The type must be an inline @import("...")@ type because ambient module
-- declarations can't reach another module through a relative import statement.
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
