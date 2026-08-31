module Wasp.Generator.SdkGenerator.Server.AuthG
  ( genServerAuth,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (Dir', File', Path', Rel, Rel', reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.AppDeliveryPlan (makeAppDeliveryPlan)
import qualified Wasp.Generator.AuthProviders as AuthProviders
import qualified Wasp.Generator.DbGenerator.Auth as DbAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Auth.Common (getOnAuthSucceededRedirectToOrDefault)
import Wasp.Generator.SdkGenerator.Common
  ( SdkTemplatesDir,
    genFileCopy,
    mkTmplFdWithData,
  )
import Wasp.Generator.SdkGenerator.Server.OAuthG (genOAuth)
import Wasp.Util ((<++>))
import qualified Wasp.Util as Util

genServerAuth :: AppSpec -> Generator [FileDraft]
genServerAuth spec =
  case maybeAuth of
    Nothing -> return []
    Just auth ->
      sequence
        [ genCoreAuth,
          genAuthIndex auth,
          genHooks auth,
          genFileCopyInServerAuth [relfile|password.ts|],
          genFileCopyInServerAuth [relfile|jwt.ts|],
          genSessionTs auth,
          genLuciaTs auth,
          genUtils auth
        ]
        <++> genAuthEmail auth
        <++> genAuthUsername auth
        <++> genOAuth (makeAppDeliveryPlan spec) auth
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

genCoreAuth :: Generator FileDraft
genCoreAuth =
  return $
    mkTmplFdWithData
      [relfile|server/core/auth.ts|]
      (object [])

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
          "isExternalAuthEnabled" .= isExternalAuthEnabled
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
          "identitiesFieldOnAuthEntityName" .= DbAuth.identitiesFieldOnAuthEntityName
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
