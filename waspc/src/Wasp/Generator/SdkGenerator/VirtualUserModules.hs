{-# LANGUAGE LambdaCase #-}

module Wasp.Generator.SdkGenerator.VirtualUserModules
  ( VirtualUserModule,
    getClientVirtualUserModules,
    getServerVirtualUserModules,
    extImportToVirtualUserModuleJsImportPath,
    mkVirtualUserModulePluginData,
    mkVirtualUserModulesDeclarationData,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, maybeToList)
import StrongPath (File', Path, Posix, Rel, relfileP)
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.App.Server as AS.App.Server
import qualified Wasp.AppSpec.ExtImport as EI
import qualified Wasp.AppSpec.Operation as AS.Operation
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.SdkGenerator.Common (SdkRootDir, getRegisteredOperationTypeName)
import Wasp.JsImport (JsImportPath (RawImportName, RelativeImportPath), getJsImportPathStringFromPath)

{-
This module allows the SDK to use the user project values (ext imports).
The other part, types, is documented in
"Wasp.Generator.TypeAugmentationGenerator.App.Sdk".

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
registered before they can be used), so it's best to avoid it.

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

For more details about the types side, check the
"Wasp.Generator.TypeAugmentationGenerator.App.Sdk".
-}

-- | Virtual user modules are virtual modules pointing to the user project files.
data VirtualUserModule = VirtualUserModule
  { -- | Runtime whose bundle the module ends up in.
    runtime :: Runtime,
    extImport :: EI.ExtImport,
    -- | Path of the module declaring 'registeredTypeName'.
    registeredTypeModule :: Path Posix (Rel SdkRootDir) File',
    -- | Type the SDK expects this module's export to have.
    registeredTypeName :: String
  }

data Runtime = ClientRuntime | ServerRuntime
  deriving (Eq)

getVirtualUserModules :: AppSpec -> [VirtualUserModule]
getVirtualUserModules spec =
  concat
    [ maybeToList $ mkClientEnvValidationSchemaModule <$> maybeClientEnvValidationSchema,
      maybeToList $ mkServerEnvValidationSchemaModule <$> maybeServerEnvValidationSchema,
      maybeToList $ mkPrismaSetupFnModule <$> maybePrismaSetupFn,
      mkAuthProviderModule <$> authProviderModules,
      mkAuthProviderUserSignupFieldsModule <$> authProviderUserSignupFields,
      mkAuthProviderSetupFnModule <$> authProviderSetupFns,
      maybeToList $ mkAuthHookModule "OnBeforeSignupHook" <$> (maybeAuth >>= AS.Auth.onBeforeSignup),
      maybeToList $ mkAuthHookModule "OnAfterSignupHook" <$> (maybeAuth >>= AS.Auth.onAfterSignup),
      maybeToList $ mkAuthHookModule "OnBeforeLoginHook" <$> (maybeAuth >>= AS.Auth.onBeforeLogin),
      maybeToList $ mkAuthHookModule "OnAfterLoginHook" <$> (maybeAuth >>= AS.Auth.onAfterLogin),
      mkAuthProviderExtensionModule <$> authProviderExtensions,
      map mkOperationModule (AS.getOperations spec)
    ]
  where
    -- The app-level lifecycle hooks are user code consumed by the SDK's
    -- choke points (provisioning, minting), so they reach the SDK through
    -- virtual modules like everything else user-authored. Several hooks
    -- typically live in one user file; TS merges the per-hook module
    -- declarations, since each declares a different export.
    mkAuthHookModule typeName extImport' =
      VirtualUserModule
        ServerRuntime
        extImport'
        [relfileP|./server/auth/hooks|]
        typeName
    mkClientEnvValidationSchemaModule extImport' =
      VirtualUserModule
        ClientRuntime
        extImport'
        [relfileP|./client/env/schema|]
        "RegisteredClientEnvValidationSchema"

    mkServerEnvValidationSchemaModule extImport' =
      VirtualUserModule
        ServerRuntime
        extImport'
        [relfileP|./server/env|]
        "RegisteredServerEnvValidationSchema"

    mkPrismaSetupFnModule extImport' =
      VirtualUserModule
        ServerRuntime
        extImport'
        [relfileP|./server/dbClient|]
        "RegisteredPrismaSetupFn"

    -- The auth provider is written in user code but consumed by the SDK's session
    -- layer, so it reaches the SDK through a virtual module like everything else
    -- user-authored. The SDK must not import user code directly. Unlike the
    -- other virtual modules, it is declared with the plain contract type rather
    -- than a Register-backed one: the SDK needs no more than `AuthProvider`,
    -- and the adapter's exact type has no consumer.
    mkAuthProviderModule extImport' =
      VirtualUserModule
        ServerRuntime
        extImport'
        [relfileP|./server/auth/provider/types|]
        "AuthProvider"

    -- Feeds just-in-time provisioning under an external provider; consumed by
    -- the SDK's session layer, so it goes through a virtual module too. Like
    -- the auth provider module, it is declared with the plain contract type:
    -- the session layer needs no more than `UserSignupFields`.
    mkAuthProviderUserSignupFieldsModule extImport' =
      VirtualUserModule
        ServerRuntime
        extImport'
        [relfileP|./auth/providers/types|]
        "UserSignupFields"

    -- The user's setup function for the adapter's underlying library
    -- (the prismaSetupFn convention); delivered to the adapter's server
    -- factory. Declared with the plain contract type: the adapter package
    -- types its parameter precisely, the SDK only needs *a* function.
    mkAuthProviderSetupFnModule extImport' =
      VirtualUserModule
        ServerRuntime
        extImport'
        [relfileP|./server/auth/provider/types|]
        "AuthProviderSetupFn"

    -- Every other user function an adapter's manifest references
    -- (`extensions`): signup field getters, OAuth config functions, email
    -- content functions, method-specific hooks. The adapter types them
    -- precisely; the SDK only forwards them, so they are declared loosely.
    mkAuthProviderExtensionModule extImport' =
      VirtualUserModule
        ServerRuntime
        extImport'
        [relfileP|./server/auth/provider/types|]
        "AuthProviderExtension"

    mkOperationModule operation =
      VirtualUserModule
        ServerRuntime
        (AS.Operation.getFn operation)
        (getOperationsIndexModulePath operation)
        (getRegisteredOperationTypeName operation)

    getOperationsIndexModulePath = \case
      AS.Operation.QueryOp _ _ -> [relfileP|./server/operations/queries/index|]
      AS.Operation.ActionOp _ _ -> [relfileP|./server/operations/actions/index|]

    maybeClientEnvValidationSchema = AS.App.client app >>= AS.App.Client.envValidationSchema
    maybeServerEnvValidationSchema = AS.App.server app >>= AS.App.Server.envValidationSchema
    maybePrismaSetupFn = AS.App.db app >>= AS.Db.prismaSetupFn
    maybeAuth = AS.App.auth app
    authProviders = maybe [] AS.Auth.providers maybeAuth
    authProviderModules = mapMaybe AS.Auth.serverModule authProviders
    authProviderUserSignupFields = mapMaybe AS.Auth.userSignupFieldsForAuthProvider authProviders
    authProviderSetupFns = mapMaybe AS.Auth.setupFn authProviders
    authProviderExtensions = concatMap (Map.elems . AS.Auth.extensions) authProviders
    app = snd $ getApp spec

-- | Virtual user modules that end up in the client bundle.
getClientVirtualUserModules :: AppSpec -> [VirtualUserModule]
getClientVirtualUserModules = filter ((== ClientRuntime) . runtime) . getVirtualUserModules

-- | Virtual user modules that end up in the server bundle.
getServerVirtualUserModules :: AppSpec -> [VirtualUserModule]
getServerVirtualUserModules = filter ((== ServerRuntime) . runtime) . getVirtualUserModules

-- | Specifier the SDK imports a user module through, e.g. @virtual:wasp/user/queries.ts@.
extImportToVirtualUserModuleJsImportPath :: EI.ExtImportPath -> JsImportPath
extImportToVirtualUserModuleJsImportPath extImportPath =
  RawImportName $ "virtual:wasp/user/" ++ SP.fromRelFileP extImportPath

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

-- | Data for virtual user modules ambient module declaration.
mkVirtualUserModulesDeclarationData :: AppSpec -> Aeson.Value
mkVirtualUserModulesDeclarationData spec =
  -- Deduplicated: two auth providers may reference the same user file and
  -- export (a shared `userSignupFields`, say). TypeScript merges ambient
  -- module blocks with distinct exports on its own, but an exact duplicate
  -- declaration would be a duplicate-identifier error.
  object ["virtualUserModules" .= nub (map mkDeclarationData (getVirtualUserModules spec))]
  where
    mkDeclarationData virtualUserModule =
      object
        [ "virtualModuleId" .= getVirtualUserModuleId virtualUserModule,
          "exportName" .= getVirtualUserModuleExportName virtualUserModule,
          "isDefaultExport" .= isDefaultExport virtualUserModule,
          "declaredType" .= getDeclaredTypeExpression virtualUserModule
        ]

    -- Name under which the user's module exports the value.
    -- We must ignore local aliases.
    getVirtualUserModuleExportName :: VirtualUserModule -> String
    getVirtualUserModuleExportName virtualUserModule = case EI.name $ extImport virtualUserModule of
      EI.ExtImportModule name -> name
      EI.ExtImportField name -> name

    isDefaultExport :: VirtualUserModule -> Bool
    isDefaultExport virtualUserModule = case EI.name $ extImport virtualUserModule of
      EI.ExtImportModule _ -> True
      EI.ExtImportField _ -> False

    -- Type the SDK expects the module's export to have, written as an inline import
    -- type (e.g. @import("./server/env").RegisteredServerEnvValidationSchema@).
    --
    -- It has to be inline because ambient module declaration can't reach another
    -- module through a relative import statement (TS2439).
    --
    -- @skipLibCheck@ tsconfig flag hides that error, so writing it as an import
    -- statement instead would silently type every declared export as @any@.
    getDeclaredTypeExpression :: VirtualUserModule -> String
    getDeclaredTypeExpression virtualUserModule =
      "import(\"" ++ getVirtualUserModuleRegisteredTypeModulePath virtualUserModule ++ "\")." ++ virtualUserModule.registeredTypeName

    getVirtualUserModuleRegisteredTypeModulePath :: VirtualUserModule -> String
    getVirtualUserModuleRegisteredTypeModulePath virtualUserModule =
      getJsImportPathStringFromPath (RelativeImportPath $ SP.castRel virtualUserModule.registeredTypeModule)

getVirtualUserModuleId :: VirtualUserModule -> String
getVirtualUserModuleId =
  getJsImportPathStringFromPath . extImportToVirtualUserModuleJsImportPath . EI.path . extImport
