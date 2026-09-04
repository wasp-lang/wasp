module Wasp.Generator.SdkGenerator.Server.AuthG
  ( genServerAuth,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import StrongPath (Dir', File', Path', Rel, Rel', reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.App.EmailSender as AS.EmailSender
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.AppSpec.Valid as AS.Valid
import Wasp.Generator.Common (makeJsArrayFromHaskellList)
import qualified Wasp.Generator.DbGenerator.Auth as DbAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common
  ( SdkTemplatesDir,
    genFileCopy,
    mkTmplFdWithData,
  )
import Wasp.Generator.SdkGenerator.JsImport (extImportToAliasedImportJson)
import qualified Wasp.Util as Util
import qualified Wasp.Util.Aeson as Util.Aeson

-- | The provider seam, the session layer, the identity store and the hook
-- dispatch: the provider-agnostic server auth surface. Every provider,
-- Wasp's own auth included, is an adapter instantiated in the registry.
genServerAuth :: AppSpec -> Generator [FileDraft]
genServerAuth spec =
  case maybeAuth of
    Nothing -> return []
    Just auth ->
      sequence
        [ genFileCopy [relfile|server/core/auth.ts|],
          genFileCopyInServerAuth [relfile|index.ts|],
          genFileCopyInServerAuth [relfile|provider/types.ts|],
          genAuthProviderIndexTs spec auth,
          genSessionTs auth,
          genSessionStoreTs auth,
          genIdentityStoreTs auth,
          genLuciaTs auth,
          genUtils auth,
          genFileCopyInServerAuth [relfile|hooks.ts|],
          genHookDispatchTs auth
        ]
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

-- | Dispatch for the app-level lifecycle hooks (`auth.hooks`): fired from the
-- SDK's provisioning and session-minting choke points, so every provider is
-- covered and none can skip them.
genHookDispatchTs :: AS.Auth.Auth -> Generator FileDraft
genHookDispatchTs auth =
  return $
    mkTmplFdWithData
      (serverAuthDirInSdkTemplatesDir </> [relfile|hookDispatch.ts|])
      tmplData
  where
    tmplData =
      object
        [ "onBeforeSignupHook" .= extImportToAliasedImportJson "onBeforeSignupHook_ext" (AS.Auth.onBeforeSignup auth),
          "onAfterSignupHook" .= extImportToAliasedImportJson "onAfterSignupHook_ext" (AS.Auth.onAfterSignup auth),
          "onBeforeLoginHook" .= extImportToAliasedImportJson "onBeforeLoginHook_ext" (AS.Auth.onBeforeLogin auth),
          "onAfterLoginHook" .= extImportToAliasedImportJson "onAfterLoginHook_ext" (AS.Auth.onAfterLogin auth)
        ]

genIdentityStoreTs :: AS.Auth.Auth -> Generator FileDraft
genIdentityStoreTs auth =
  return $
    mkTmplFdWithData
      (serverAuthDirInSdkTemplatesDir </> [relfile|identityStore.ts|])
      tmplData
  where
    tmplData =
      object
        [ "userEntityUpper" .= (userEntityName :: String),
          "userEntityLower" .= (Util.toLowerFirst userEntityName :: String),
          "authEntityUpper" .= (DbAuth.authEntityName :: String),
          "authIdentityEntityLower" .= (Util.toLowerFirst DbAuth.authIdentityEntityName :: String),
          "authFieldOnUserEntityName" .= (DbAuth.authFieldOnUserEntityName :: String),
          "identitiesFieldOnAuthEntityName" .= (DbAuth.identitiesFieldOnAuthEntityName :: String)
        ]
    userEntityName = AS.refName $ AS.Auth.userEntity auth

genSessionStoreTs :: AS.Auth.Auth -> Generator FileDraft
genSessionStoreTs _ =
  return $
    mkTmplFdWithData
      (serverAuthDirInSdkTemplatesDir </> [relfile|sessionStore.ts|])
      tmplData
  where
    tmplData =
      object
        [ "sessionEntityLower" .= (Util.toLowerFirst DbAuth.sessionEntityName :: String),
          "sessionEntityUpper" .= (DbAuth.sessionEntityName :: String)
        ]

genLuciaTs :: AS.Auth.Auth -> Generator FileDraft
genLuciaTs auth =
  return $
    mkTmplFdWithData
      (serverAuthDirInSdkTemplatesDir </> [relfile|lucia.ts|])
      tmplData
  where
    tmplData =
      object
        [ "sessionEntityLower" .= (Util.toLowerFirst DbAuth.sessionEntityName :: String),
          "authEntityLower" .= (Util.toLowerFirst DbAuth.authEntityName :: String),
          "userEntityUpper" .= (userEntityName :: String)
        ]

    userEntityName = AS.refName $ AS.Auth.userEntity auth

-- | The provider registry the app runs on: one entry per configured provider,
-- keyed by provider id. Adapter packages are instantiated here (each with the
-- identity store pre-bound to its own id), user-module providers are imported
-- through virtual user modules.
genAuthProviderIndexTs :: AppSpec -> AS.Auth.Auth -> Generator FileDraft
genAuthProviderIndexTs spec auth =
  return $
    mkTmplFdWithData
      (serverAuthDirInSdkTemplatesDir </> [relfile|provider/index.ts|])
      tmplData
  where
    tmplData =
      object
        [ "dbProvider" .= prismaDbProviderName,
          "authFieldOnUserEntityName" .= DbAuth.authFieldOnUserEntityName,
          -- The email-send grant can only be wired when the app has an email
          -- sender; validation guarantees no manifest requests it otherwise.
          "isEmailSenderEnabled" .= isJust maybeEmailSender,
          "defaultFromJson"
            .= maybe "undefined" Util.Aeson.encodeToString (AS.EmailSender.defaultFrom =<< maybeEmailSender),
          "authProviders" .= mkAuthProvidersTmplData auth
        ]
    maybeEmailSender = AS.App.emailSender $ snd $ AS.Valid.getApp spec
    prismaDbProviderName :: String
    prismaDbProviderName = case AS.Valid.getValidDbSystem spec of
      AS.Db.PostgreSQL -> "postgresql"
      AS.Db.SQLite -> "sqlite"

-- | Per-provider template data, in declaration order. Import aliases carry
-- the provider's index so that two providers whose user modules share an
-- export name never collide in one generated file.
mkAuthProvidersTmplData :: AS.Auth.Auth -> [Aeson.Value]
mkAuthProvidersTmplData auth =
  zipWith mkProviderTmplData [0 :: Int ..] (AS.Auth.providers auth)
  where
    mkProviderTmplData idx provider =
      object
        [ "index" .= idx,
          "providerId" .= provider.providerId,
          "isPackage" .= isJust (AS.Auth.serverPackage provider),
          "serverPackage" .= AS.Auth.serverPackage provider,
          "providerModule"
            .= extImportToAliasedImportJson ("authProviderModule_" ++ show idx) (AS.Auth.serverModule provider),
          -- The adapter's serializable options, spliced in verbatim -- the
          -- mapper already proved the text is valid JSON.
          "optionsJson" .= fromMaybe "undefined" provider.optionsJson,
          "setupFn"
            .= extImportToAliasedImportJson ("authProviderSetupFn_" ++ show idx) provider.setupFn,
          "userSignupFields"
            .= extImportToAliasedImportJson
              ("authProviderUserSignupFields_" ++ show idx)
              (AS.Auth.userSignupFieldsForAuthProvider provider),
          -- Every other user function the manifest referenced, delivered to
          -- the adapter's server factory under the name it expects.
          "extensions"
            .= [ object
                   [ "name" .= name,
                     "import" .= extImportToAliasedImportJson ("authProviderExtension_" ++ show idx ++ "_" ++ name) (Just extImport)
                   ]
               | (name, extImport) <- Map.toList provider.extensions
               ],
          -- The manifest's compile-time claims, checked against the runtime
          -- adapter object at boot so a wrong manifest fails loudly instead of
          -- generating a surface the adapter cannot back.
          "capabilitiesJs" .= makeJsArrayFromHaskellList provider.capabilities,
          -- The adapter runtime's env is narrowed to exactly these names.
          "serverEnvVarNamesJs"
            .= makeJsArrayFromHaskellList ((.name) <$> provider.envVars.server),
          -- The runtime facets the manifest requested; only these get wired.
          "usesJs" .= makeJsArrayFromHaskellList provider.uses,
          "identityNamespacesJs" .= makeJsArrayFromHaskellList provider.identityNamespaces
        ]

genSessionTs :: AS.Auth.Auth -> Generator FileDraft
genSessionTs auth =
  return $
    mkTmplFdWithData
      (serverAuthDirInSdkTemplatesDir </> [relfile|session.ts|])
      tmplData
  where
    tmplData =
      object
        [ "userEntityUpper" .= userEntityName,
          "userEntityLower" .= Util.toLowerFirst userEntityName,
          "authFieldOnUserEntityName" .= DbAuth.authFieldOnUserEntityName,
          "authIdentityEntityLower" .= Util.toLowerFirst DbAuth.authIdentityEntityName,
          "identitiesFieldOnAuthEntityName" .= DbAuth.identitiesFieldOnAuthEntityName,
          "authProviders" .= mkAuthProvidersTmplData auth
        ]
    userEntityName = AS.refName $ AS.Auth.userEntity auth

genUtils :: AS.Auth.Auth -> Generator FileDraft
genUtils auth =
  return $
    mkTmplFdWithData
      (serverAuthDirInSdkTemplatesDir </> [relfile|utils.ts|])
      tmplData
  where
    tmplData =
      object
        [ "userEntityUpper" .= (userEntityName :: String),
          "userEntityLower" .= (Util.toLowerFirst userEntityName :: String),
          "authEntityUpper" .= (DbAuth.authEntityName :: String),
          "authEntityLower" .= (Util.toLowerFirst DbAuth.authEntityName :: String),
          "userFieldOnAuthEntityName" .= (DbAuth.userFieldOnAuthEntityName :: String),
          "authIdentityEntityUpper" .= (DbAuth.authIdentityEntityName :: String),
          "authIdentityEntityLower" .= (Util.toLowerFirst DbAuth.authIdentityEntityName :: String),
          "authFieldOnUserEntityName" .= (DbAuth.authFieldOnUserEntityName :: String),
          "identitiesFieldOnAuthEntityName" .= (DbAuth.identitiesFieldOnAuthEntityName :: String),
          "failureRedirectPath" .= AS.Auth.onAuthFailedRedirectTo auth
        ]
    userEntityName = AS.refName $ AS.Auth.userEntity auth

serverAuthDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) Dir'
serverAuthDirInSdkTemplatesDir = [reldir|server/auth|]

genFileCopyInServerAuth :: Path' Rel' File' -> Generator FileDraft
genFileCopyInServerAuth =
  genFileCopy . (serverAuthDirInSdkTemplatesDir </>)
