{-# LANGUAGE LambdaCase #-}

module Wasp.Generator.SdkGenerator.VirtualUserModules
  ( VirtualUserModule (..),
    getClientVirtualUserModules,
    getServerVirtualUserModules,
    getVirtualUserModuleId,
    getVirtualUserModuleJsImportPath,
    getVirtualUserModuleExportName,
    getDeclaredTypeExpression,
    isDefaultExport,
    mkVirtualUserModulePluginData,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (maybeToList)
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.App.Server as AS.App.Server
import qualified Wasp.AppSpec.ExtImport as EI
import qualified Wasp.AppSpec.Operation as AS.Operation
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.SdkGenerator.Common (getRegisteredOperationTypeName)
import Wasp.JsImport (JsImportPath (RawImportName), getJsImportPathStringFromPath)

{-
This module allows the SDK to use the user project values (ext imports).
The other part, types, is document in @sdk/wasp/types/register.ts@.

If the SDK tried to import values from the user project directly,
it would create a cyclic dependency between TypeScript projects.
TypeScript can't compile projects that have cyclic dependencies.
So we must find a way to use the user project values without the
SDK depending on the user project.

Copying the user project and making the SDK depend on the copy
is not an option, because it forces the user's project to compile
with the SDK's TypeScript config.
(Old issue about the problem: https://github.com/wasp-lang/wasp/issues/2247)

The direct way to do that would be DI (dependency injection).
However, DI is fragile to module initialization order (values must be
registered before they can be used), so we want to avoid it.

Instead, the solution is to proxy the user project imports through
virtual modules. Bundler resolves virtual user modules at bundle time,
which happens after all TypeScript projects have been compiled.
This delays the resolution of the SDK's user project imports until
after compilation, so the compiler never sees a cycle.

For virtual modules to be resolved properly, each runtime that uses
the SDK (the server and the client) must equip its bundler with a
plugin that resolves virtual user modules into actual user files.

Each plugin resolves only the virtual user modules that end up in its
runtime's bundle. E.g., the server plugin resolves server-side user modules
like operations, but not the client env validation schema.

While this makes everything work at runtime, TypeScript compilation
would still fail because the virtual modules don't exist at compile
time. To satisfy TypeScript we have to do module declaration for every
virtual user module the SDK uses:

> // ./sdk/wasp/wasp-user-virtual-modules.d.ts
> declare module "virtual:wasp/user/env" {
>   export const clientEnvValidationSchema: import("./client/env/schema").RegisteredClientEnvValidationSchema;
> }
> // Many other declarations...

See 'Wasp.Generator.SdkGenerator.VirtualUserModules' for more details.
-}

-- | Virtual user modules are virtual modules pointing to the user's project files.
--
-- For a virtual user module to work, we need to:
-- 1. Register it at the client or server bundler plugin (whichever it is
--    supposed to end up in), which resolves its module ID to a user file.
-- 2. Generate an ambient module declaration for it in
--    @wasp-user-virtual-modules.d.ts@.
data VirtualUserModule = VirtualUserModule
  { extImport :: EI.ExtImport,
    -- | Path, relative to the SDK root, of the module declaring 'declaredTypeName'.
    declaredTypeModulePath :: String,
    -- | Type the SDK expects this module's export to have.
    declaredTypeName :: String
  }

-- | Virtual user modules that end up in the client bundle.
getClientVirtualUserModules :: AppSpec -> [VirtualUserModule]
getClientVirtualUserModules spec =
  maybeToList $ mkEnvValidationSchemaModule <$> maybeClientEnvValidationSchema
  where
    mkEnvValidationSchemaModule extImport' =
      VirtualUserModule
        extImport'
        "./client/env/schema"
        "RegisteredClientEnvValidationSchema"

    maybeClientEnvValidationSchema = AS.App.client app >>= AS.App.Client.envValidationSchema
    app = snd $ getApp spec

-- | Virtual user modules that end up in the server bundle.
getServerVirtualUserModules :: AppSpec -> [VirtualUserModule]
getServerVirtualUserModules spec =
  maybeToList (mkEnvValidationSchemaModule <$> maybeServerEnvValidationSchema)
    ++ maybeToList (mkPrismaSetupFnModule <$> maybePrismaSetupFn)
    ++ map mkOperationModule (AS.getOperations spec)
  where
    mkEnvValidationSchemaModule extImport' =
      VirtualUserModule
        extImport'
        "./server/env"
        "RegisteredServerEnvValidationSchema"

    mkPrismaSetupFnModule extImport' =
      VirtualUserModule
        extImport'
        "./server/dbClient"
        "RegisteredPrismaSetupFn"

    mkOperationModule operation =
      VirtualUserModule
        (AS.Operation.getFn operation)
        (getOperationsIndexModulePath operation)
        (getRegisteredOperationTypeName operation)

    getOperationsIndexModulePath = \case
      AS.Operation.QueryOp _ _ -> "./server/operations/queries/index"
      AS.Operation.ActionOp _ _ -> "./server/operations/actions/index"

    maybeServerEnvValidationSchema = AS.App.server app >>= AS.App.Server.envValidationSchema
    maybePrismaSetupFn = AS.App.db app >>= AS.Db.prismaSetupFn
    app = snd $ getApp spec

getVirtualUserModuleId :: VirtualUserModule -> String
getVirtualUserModuleId =
  getJsImportPathStringFromPath . getVirtualUserModuleJsImportPath . EI.path . extImport

-- | Specifier the SDK imports a user module through, e.g. @virtual:wasp/user/queries.ts@.
getVirtualUserModuleJsImportPath :: EI.ExtImportPath -> JsImportPath
getVirtualUserModuleJsImportPath extImportPath =
  RawImportName $ "virtual:wasp/user/" ++ SP.fromRelFileP extImportPath

-- | Name under which the user's module exports the value.
-- We must ignore local aliases.
getVirtualUserModuleExportName :: VirtualUserModule -> String
getVirtualUserModuleExportName virtualUserModule = case EI.name $ extImport virtualUserModule of
  EI.ExtImportModule name -> name
  EI.ExtImportField name -> name

-- | Type the SDK expects the module's export to have, written as an inline import
-- type (e.g. @import("./server/env").RegisteredServerEnvValidationSchema@).
--
-- It has to be inline because ambient module declaration can't reach another
-- module through a relative import statement (TS2439).
--
-- @skipLibCheck@ tsconfig flag hides that error, so writing it as an import
-- statement instead would silently type every declared export as @any@.
getDeclaredTypeExpression :: VirtualUserModule -> String
getDeclaredTypeExpression virtualUserModule =
  "import(\"" ++ declaredTypeModulePath virtualUserModule ++ "\")." ++ declaredTypeName virtualUserModule

isDefaultExport :: VirtualUserModule -> Bool
isDefaultExport virtualUserModule = case EI.name $ extImport virtualUserModule of
  EI.ExtImportModule _ -> True
  EI.ExtImportField _ -> False

-- | Data for one entry of a bundler plugin's virtual module ID to user file map.
mkVirtualUserModulePluginData ::
  (EI.ExtImport -> Aeson.Value) ->
  VirtualUserModule ->
  Aeson.Value
mkVirtualUserModulePluginData extImportToImportJson virtualUserModule =
  object
    [ "virtualModuleId" .= getVirtualUserModuleId virtualUserModule,
      "importJson" .= extImportToImportJson (extImport virtualUserModule)
    ]
