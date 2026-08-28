module Wasp.Generator.TypeAugmentationGenerator.App.Sdk
  ( genSdkTypeAugmentation,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson.Types as Aeson.Types
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec, getCruds, getOperations)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.App.Server as AS.App.Server
import qualified Wasp.AppSpec.App.WebSocket as AS.App.WS
import qualified Wasp.AppSpec.Crud as AS.Crud
import qualified Wasp.AppSpec.Operation as AS.Operation
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.Crud (makeCrudOperationKeyAndJsonPair)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.TypeAugmentationGenerator.App.Sdk.Common (mkTmplFdWithData)
import Wasp.Generator.TypeAugmentationGenerator.App.Sdk.JsImport (extImportToImportJson, extOperationImportToImportJson)

{-
This module allows the SDK to use the user project types.
The other part, runtime values, is supplied through the generated server
runtime bindings.

If the SDK tried to import types from the user project directly,
it would create a cyclic dependency between TypeScript projects.
TypeScript can't compile projects that have cyclic dependencies.
So we must find a way to use the user project types without the
SDK depending on the user project.

Copying the user project and making the SDK depend on the copy
is not an option, because it forces the user's project to compile
with the SDK's TypeScript config.
(Old issue about the problem: https://github.com/wasp-lang/wasp/issues/2247)

Instead, the SDK defines an extension point (empty @Register@ interface,
declared in @sdk/wasp/types/register.ts@) that the user project extends.
The SDK never imports from the user project, it only references the
extension point, which resolves to the user's types when TypeScript
compiles the user project.

@Register@ is publicly exported through the @wasp/types@ module.
During compilation, Wasp generates type declarations in
@.wasp/out/types/app/sdk/register.ts@ (part of the user project) that
extend @Register@ via module augmentation and declaration merging.

On the SDK side, all user project dependent types are defined as
conditional types. If a user-defined type for something exists in
@Register@, we use it; otherwise, we fallback to some sensible
default type.

The purpose of conditional types in SDK is two-fold:

1. They allow the SDK to compile on its own (without a user project).
   The SDK compiles before any other TypeScript project, so at that
   point @Register@ is always empty and every conditional type
   resolves to its fallback. By having the fallback types satisfy
   everything the SDK expects of them, the SDK can compile on its own.

   E.g., during SDK compilation, the @PrismaClient@ type resolves to
   its fallback: a Prisma client with default settings. This satisfies
   all SDK's expectations of the @PrismaClient@ type.

2. They let the same SDK declarations resolve to different types in
   different projects. We force SDK's emitted declarations to stay
   in their conditional (rather than resolved) form, so when
   TypeScript checks the user project, where @Register@ is extended,
   those same conditional types resolve to the user's types instead
   of the fallbacks. It also means that as soon as users update their
   types, the SDK's types recalculate.

   E.g., if a user defines a custom Prisma client instance, the
   @PrismaClient@ type will instead return the user's custom client.

Above we said we "force the types to stay in their conditional (rather
than resolved) form". Why do we have to force them?

TypeScript resolves inferred types when emitting declaration files.
Since the SDK compiles with an empty @Register@, an inferred type
ends up in the emitted @.d.ts@ file as the already-resolved fallback:

> // Inferred: emitted already resolved to the fallback.
> declare const dbClient: InternalPrismaClient;

Therefore, everything in the SDK derived from a registered type must be
explicitly typed (@: RegisteredType@, @as RegisteredType@, or
@<T extends RegisteredType>@) to force TypeScript to keep its conditional
form in the emitted declarations:

> // Explicitly typed: emitted in its conditional form.
> declare const dbClient: ReturnType<FromRegister<'prismaSetupFn', () => InternalPrismaClient>>;

PR implementing the change: https://github.com/wasp-lang/wasp/pull/4049
-}

genSdkTypeAugmentation :: AppSpec -> Generator [FileDraft]
genSdkTypeAugmentation spec =
  return
    [ mkTmplFdWithData
        [relfile|register.ts|]
        tmplData
    ]
  where
    tmplData =
      object
        [ "serverEnvValidationSchema" .= extImportToImportJson (AS.App.server app >>= AS.App.Server.envValidationSchema),
          "clientEnvValidationSchema" .= extImportToImportJson (AS.App.client app >>= AS.App.Client.envValidationSchema),
          "webSocketFn" .= extImportToImportJson (AS.App.WS.fn <$> AS.App.webSocket app),
          "prismaSetupFn" .= extImportToImportJson (AS.App.db app >>= AS.Db.prismaSetupFn),
          "emailUserSignupFields" .= extImportToImportJson (authMethods >>= AS.Auth.email >>= AS.Auth.userSignupFieldsForEmailAuth),
          "usernameAndPasswordUserSignupFields" .= extImportToImportJson (authMethods >>= AS.Auth.usernameAndPassword >>= AS.Auth.userSignupFieldsForUsernameAuth),
          "operations" .= map mkOperationData operations,
          "cruds" .= map mkCrudData cruds
        ]
    authMethods = AS.Auth.methods <$> maybeAuth
    maybeAuth = AS.App.auth app
    app = snd $ getApp spec
    cruds = getCruds spec
    operations = getOperations spec

    mkOperationData :: AS.Operation.Operation -> Aeson.Types.Value
    mkOperationData operation =
      object
        [ "jsFn" .= extOperationImportToImportJson (AS.Operation.getFn operation),
          "operationName" .= AS.Operation.getName operation
        ]

    mkCrudData :: (String, AS.Crud.Crud) -> Aeson.Types.Value
    mkCrudData (name, crud) =
      object
        [ "name" .= name,
          "overrides" .= object (map operationToOverrideImport (AS.Crud.toOperationList crud.operations))
        ]

    operationToOverrideImport :: (AS.Crud.CrudOperation, AS.Crud.CrudOperationOptions) -> Aeson.Types.Pair
    operationToOverrideImport (operation, options) =
      makeCrudOperationKeyAndJsonPair operation (extImportToImportJson (AS.Crud.overrideFn options))
