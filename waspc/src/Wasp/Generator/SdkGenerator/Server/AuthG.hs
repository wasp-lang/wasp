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
            genUtils auth
          ]
            ++ ( if AS.Auth.isWaspAuthProviderUsed auth
                   then
                     [ genHooks auth,
                       genFileCopyInServerAuth [relfile|password.ts|],
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
          "externalAuthProviders" .= mkExternalAuthProvidersTmplData auth
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
          "capabilitiesJs" .= makeJsArrayFromHaskellList extProvider.capabilities
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
