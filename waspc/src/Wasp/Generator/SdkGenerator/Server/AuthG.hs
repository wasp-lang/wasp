module Wasp.Generator.SdkGenerator.Server.AuthG
  ( genServerAuth,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
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
import qualified Wasp.Generator.AuthProviders as AuthProviders
import Wasp.Generator.Common (makeJsArrayFromHaskellList)
import qualified Wasp.Generator.DbGenerator.Auth as DbAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Auth.Common (getOnAuthSucceededRedirectToOrDefault)
import Wasp.Generator.SdkGenerator.Common
  ( SdkTemplatesDir,
    genFileCopy,
    mkTmplFdWithData,
  )
import Wasp.Generator.SdkGenerator.JsImport (extImportToAliasedImportJson)
import Wasp.Generator.SdkGenerator.Server.OAuthG (genOAuth)
import Wasp.Util ((<++>))
import qualified Wasp.Util as Util
import qualified Wasp.Util.Aeson as Util.Aeson

genServerAuth :: AppSpec -> Generator [FileDraft]
genServerAuth spec =
  case maybeAuth of
    Nothing -> return []
    -- The provider seam, the session layer, and the identity store exist for
    -- every provider mix. Everything password-shaped -- hashing, jwt, Wasp's
    -- own provider, hooks -- exists iff Wasp's own auth is among the
    -- providers, so it cannot be imported (and its dependencies are not
    -- installed) in externals-only apps.
    Just auth ->
      sequence
        ( [ genFileCopy [relfile|server/core/auth.ts|],
            genAuthIndex auth,
            genFileCopyInServerAuth [relfile|provider/types.ts|],
            genAuthProviderIndexTs spec auth,
            genSessionTs auth,
            genSessionStoreTs auth,
            genIdentityStoreTs auth,
            genLuciaTs auth,
            genUtils auth,
            -- Hook types and dispatch exist for every provider mix: the
            -- app-level lifecycle hooks fire at Wasp-owned choke points
            -- (provisioning, minting), whichever provider triggers them.
            genHooks auth,
            genHookDispatchTs auth
          ]
            ++ ( if AS.Auth.isWaspAuthProviderUsed auth
                   then
                     [ genFileCopyInServerAuth [relfile|password.ts|],
                       genFileCopyInServerAuth [relfile|jwt.ts|],
                       genFileCopyInServerAuth [relfile|provider/wasp.ts|]
                     ]
                   else []
               )
        )
        <++> genAuthEmail auth
        <++> genAuthUsername auth
        <++> genOAuth auth
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

genAuthIndex :: AS.Auth.Auth -> Generator FileDraft
genAuthIndex auth =
  return $
    mkTmplFdWithData
      (serverAuthDirInSdkTemplatesDir </> [relfile|index.ts|])
      tmplData
  where
    tmplData =
      object
        [ "enabledProviders" .= AuthProviders.getEnabledAuthProvidersJson auth,
          "isExternalAuthEnabled" .= isExternalAuthEnabled,
          "isWaspAuthProviderUsed" .= AS.Auth.isWaspAuthProviderUsed auth,
          "anyExternalProvidersUsed" .= AS.Auth.isExternalAuthProviderUsed auth
        ]
    isExternalAuthEnabled = AS.Auth.isExternalAuthEnabled auth

genHooks :: AS.Auth.Auth -> Generator FileDraft
genHooks auth =
  return $
    mkTmplFdWithData
      (serverAuthDirInSdkTemplatesDir </> [relfile|hooks.ts|])
      tmplData
  where
    tmplData = object ["enabledProviders" .= AuthProviders.getEnabledAuthProvidersJson auth]

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
-- through virtual user modules, and Wasp's own auth joins under the 'wasp' id.
genAuthProviderIndexTs :: AppSpec -> AS.Auth.Auth -> Generator FileDraft
genAuthProviderIndexTs spec auth =
  return $
    mkTmplFdWithData
      (serverAuthDirInSdkTemplatesDir </> [relfile|provider/index.ts|])
      tmplData
  where
    tmplData =
      object
        [ "isWaspAuthProviderUsed" .= AS.Auth.isWaspAuthProviderUsed auth,
          "anyExternalProvidersUsed" .= AS.Auth.isExternalAuthProviderUsed auth,
          "dbProvider" .= prismaDbProviderName,
          "authFieldOnUserEntityName" .= DbAuth.authFieldOnUserEntityName,
          -- The email-send grant can only be wired when the app has an email
          -- sender; validation guarantees no manifest requests it otherwise.
          "isEmailSenderEnabled" .= isJust maybeEmailSender,
          "defaultFromJson"
            .= maybe "undefined" Util.Aeson.encodeToString (AS.EmailSender.defaultFrom =<< maybeEmailSender),
          -- The identity namespaces of Wasp's own auth: one per enabled
          -- method, unprefixed by the wasp provider's compatibility privilege
          -- (released AuthIdentity rows carry these providerName values).
          "waspIdentityNamespacesJs" .= makeJsArrayFromHaskellList waspIdentityNamespaces,
          "externalAuthProviders" .= mkExternalAuthProvidersTmplData auth
        ]
    maybeEmailSender = AS.App.emailSender $ snd $ AS.Valid.getApp spec
    waspIdentityNamespaces =
      concat
        [ ["wasp"],
          ["username" | AS.Auth.isUsernameAndPasswordAuthEnabled auth],
          ["email" | AS.Auth.isEmailAuthEnabled auth],
          ["google" | AS.Auth.isGoogleAuthEnabled auth],
          ["github" | AS.Auth.isGitHubAuthEnabled auth],
          ["keycloak" | AS.Auth.isKeycloakAuthEnabled auth],
          ["slack" | AS.Auth.isSlackAuthEnabled auth],
          ["discord" | AS.Auth.isDiscordAuthEnabled auth],
          ["microsoft" | AS.Auth.isMicrosoftAuthEnabled auth]
        ]
    prismaDbProviderName :: String
    prismaDbProviderName = case AS.Valid.getValidDbSystem spec of
      AS.Db.PostgreSQL -> "postgresql"
      AS.Db.SQLite -> "sqlite"

-- | Per-external-provider template data, in declaration order. Import aliases
-- carry the provider's index so that two providers whose user modules share an
-- export name never collide in one generated file.
mkExternalAuthProvidersTmplData :: AS.Auth.Auth -> [Aeson.Value]
mkExternalAuthProvidersTmplData auth =
  zipWith mkProviderTmplData [0 :: Int ..] (AS.Auth.externalProviders auth)
  where
    mkProviderTmplData idx extProvider =
      object
        [ "index" .= idx,
          "providerId" .= extProvider.providerId,
          "isPackage" .= isJust (AS.Auth.serverPackage extProvider),
          "serverPackage" .= AS.Auth.serverPackage extProvider,
          "providerModule"
            .= extImportToAliasedImportJson ("authProviderModule_" ++ show idx) (AS.Auth.serverModule extProvider),
          -- The adapter's serializable options, spliced in verbatim -- the
          -- mapper already proved the text is valid JSON.
          "optionsJson" .= fromMaybe "undefined" extProvider.optionsJson,
          "setupFn"
            .= extImportToAliasedImportJson ("authProviderSetupFn_" ++ show idx) extProvider.setupFn,
          "userSignupFields"
            .= extImportToAliasedImportJson
              ("authProviderUserSignupFields_" ++ show idx)
              (AS.Auth.userSignupFieldsForExternalAuthProvider extProvider),
          -- The manifest's compile-time claims, checked against the runtime
          -- adapter object at boot so a wrong manifest fails loudly instead of
          -- generating a surface the adapter cannot back.
          "capabilitiesJs" .= makeJsArrayFromHaskellList extProvider.capabilities,
          -- The adapter runtime's env is narrowed to exactly these names.
          "serverEnvVarNamesJs"
            .= makeJsArrayFromHaskellList ((.name) <$> extProvider.envVars.server),
          -- The runtime facets the manifest requested; only these get wired.
          "usesJs" .= makeJsArrayFromHaskellList extProvider.uses,
          "identityNamespacesJs" .= makeJsArrayFromHaskellList extProvider.identityNamespaces
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
          -- Just-in-time provisioning only exists for external providers.
          -- Emitting it unconditionally breaks apps whose user entity has
          -- required fields, because the provisioning insert supplies none of
          -- them.
          "anyExternalProvidersUsed" .= AS.Auth.isExternalAuthProviderUsed auth,
          "externalAuthProviders" .= mkExternalAuthProvidersTmplData auth
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
          "failureRedirectPath" .= AS.Auth.onAuthFailedRedirectTo auth,
          "successRedirectPath" .= getOnAuthSucceededRedirectToOrDefault auth
        ]
    userEntityName = AS.refName $ AS.Auth.userEntity auth

genAuthEmail :: AS.Auth.Auth -> Generator [FileDraft]
genAuthEmail auth =
  if AS.Auth.isEmailAuthEnabled auth
    then
      sequence
        [ genFileCopyInServerAuth [relfile|email/index.ts|],
          genEmailUtils auth
        ]
    else return []

genEmailUtils :: AS.Auth.Auth -> Generator FileDraft
genEmailUtils auth =
  return $
    mkTmplFdWithData
      (serverAuthDirInSdkTemplatesDir </> [relfile|email/utils.ts|])
      tmplData
  where
    tmplData =
      object
        [ "userEntityUpper" .= (userEntityName :: String),
          "userEntityLower" .= (Util.toLowerFirst userEntityName :: String),
          "authEntityUpper" .= (DbAuth.authEntityName :: String),
          "authEntityLower" .= (Util.toLowerFirst DbAuth.authEntityName :: String),
          "userFieldOnAuthEntityName" .= (DbAuth.userFieldOnAuthEntityName :: String)
        ]
    userEntityName = AS.refName $ AS.Auth.userEntity auth

genAuthUsername :: AS.Auth.Auth -> Generator [FileDraft]
genAuthUsername auth =
  if AS.Auth.isUsernameAndPasswordAuthEnabled auth
    then sequence [genFileCopyInServerAuth [relfile|username.ts|]]
    else return []

serverAuthDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) Dir'
serverAuthDirInSdkTemplatesDir = [reldir|server/auth|]

genFileCopyInServerAuth :: Path' Rel' File' -> Generator FileDraft
genFileCopyInServerAuth =
  genFileCopy . (serverAuthDirInSdkTemplatesDir </>)
