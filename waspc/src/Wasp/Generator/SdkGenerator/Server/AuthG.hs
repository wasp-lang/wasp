module Wasp.Generator.SdkGenerator.Server.AuthG
  ( genServerAuth,
  )
where

import Data.Aeson (object, (.=))
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
import Wasp.Generator.SdkGenerator.JsImport (extImportToImportJson)
import Wasp.Generator.SdkGenerator.Server.OAuthG (genOAuth)
import Wasp.Util ((<++>))
import qualified Wasp.Util as Util

genServerAuth :: AppSpec -> Generator [FileDraft]
genServerAuth spec =
  case maybeAuth of
    Nothing -> return []
    -- Under an external provider only the provider seam and the session read
    -- path exist. Everything password-shaped -- lucia, hashing, jwt, Wasp's own
    -- provider -- is not generated, so it cannot be imported and its
    -- dependencies are not installed.
    Just auth
      | AS.Auth.isExternalAuthProviderUsed auth ->
          -- Wasp's session store backs external providers too: their credential
          -- is exchanged for a Wasp session at login, so the store (and lucia
          -- behind it) is generated. Everything password-shaped -- hashing,
          -- jwt, Wasp's own provider -- still is not.
          sequence
            [ genFileCopy [relfile|server/core/auth.ts|],
              genAuthIndex auth,
              genFileCopyInServerAuth [relfile|provider/types.ts|],
              genAuthProviderIndexTs spec auth,
              genSessionTs auth,
              genSessionStoreTs auth,
              genIdentityStoreTs auth,
              genLuciaTs auth,
              genUtils auth
            ]
      | otherwise ->
          sequence
            [ genFileCopy [relfile|server/core/auth.ts|],
              genAuthIndex auth,
              genHooks auth,
              genFileCopyInServerAuth [relfile|password.ts|],
              genFileCopyInServerAuth [relfile|jwt.ts|],
              genFileCopyInServerAuth [relfile|provider/types.ts|],
              genFileCopyInServerAuth [relfile|provider/wasp.ts|],
              genAuthProviderIndexTs spec auth,
              genSessionTs auth,
              genSessionStoreTs auth,
              genIdentityStoreTs auth,
              genLuciaTs auth,
              genUtils auth
            ]
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
          "isCustomAuthProviderUsed" .= AS.Auth.isExternalAuthProviderUsed auth
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

-- | Selects the auth provider the app runs on.
--
-- Defaults to Wasp's own auth. When @app.auth.provider@ is set, the SDK imports the
-- developer's adapter through a virtual user module instead, and the session layer
-- switches to resolving foreign subjects (provisioning a local user on first sight).
genAuthProviderIndexTs :: AppSpec -> AS.Auth.Auth -> Generator FileDraft
genAuthProviderIndexTs spec auth =
  return $
    mkTmplFdWithData
      (serverAuthDirInSdkTemplatesDir </> [relfile|provider/index.ts|])
      tmplData
  where
    tmplData =
      object
        [ "isCustomAuthProviderUsed" .= isJust maybeExternalProvider,
          "isPackageAuthProvider" .= isJust maybeServerPackage,
          "serverPackage" .= maybeServerPackage,
          "authProvider" .= extImportToImportJson maybeProviderModule,
          -- The adapter's serializable options, spliced in verbatim -- the
          -- mapper already proved the text is valid JSON.
          "optionsJson" .= fromMaybe "undefined" (AS.Auth.optionsJson =<< maybeExternalProvider),
          "dbProvider" .= prismaDbProviderName,
          -- The manifest's compile-time claims, checked against the runtime
          -- adapter object at boot so a wrong manifest fails loudly instead of
          -- generating a surface the adapter cannot back.
          "externalSetupFn"
            .= extImportToImportJson (AS.Auth.setupFn =<< maybeExternalProvider),
          "manifestProviderId" .= (AS.Auth.providerId <$> maybeExternalProvider),
          "manifestCapabilities" .= (makeJsArrayFromHaskellList . AS.Auth.capabilities <$> maybeExternalProvider)
        ]
    maybeProviderModule = AS.Auth.serverModule =<< maybeExternalProvider
    maybeServerPackage = AS.Auth.serverPackage =<< maybeExternalProvider
    maybeExternalProvider = AS.Auth.externalProvider auth
    prismaDbProviderName :: String
    prismaDbProviderName = case AS.Valid.getValidDbSystem spec of
      AS.Db.PostgreSQL -> "postgresql"
      AS.Db.SQLite -> "sqlite"

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
          -- Just-in-time provisioning only exists for providers that don't own Wasp's
          -- auth entity. Emitting it unconditionally breaks apps whose user entity has
          -- required fields, because the provisioning insert supplies none of them.
          "isCustomAuthProviderUsed" .= AS.Auth.isExternalAuthProviderUsed auth,
          "externalUserSignupFields"
            .= extImportToImportJson (AS.Auth.userSignupFieldsForExternalAuthProvider =<< AS.Auth.externalProvider auth)
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
          "successRedirectPath" .= getOnAuthSucceededRedirectToOrDefault auth,
          "isCustomAuthProviderUsed" .= AS.Auth.isExternalAuthProviderUsed auth
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
