module Wasp.Generator.SdkGenerator.Server.AuthG
  ( genNewServerApi,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.Generator.AuthProviders as AuthProviders
import qualified Wasp.Generator.DbGenerator.Auth as DbAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.UserCore.Common
  ( mkTmplFd,
    mkTmplFdWithData,
  )
import Wasp.Util ((<++>))

genNewServerApi :: AppSpec -> Generator [FileDraft]
genNewServerApi spec =
  case maybeAuth of
    Nothing -> return []
    Just auth ->
      sequence
        [ genAuthIndex auth,
          genAuthUser auth,
          genHooks auth
        ]
        <++> genAuthEmail auth
        <++> genAuthUsername auth
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

genAuthIndex :: AS.Auth.Auth -> Generator FileDraft
genAuthIndex auth =
  return $ mkTmplFdWithData [relfile|server/auth/index.ts|] tmplData
  where
    tmplData =
      object
        [ "enabledProviders" .= AuthProviders.getEnabledAuthProvidersJson auth,
          "isExternalAuthEnabled" .= isExternalAuthEnabled
        ]
    isExternalAuthEnabled = AS.Auth.isExternalAuthEnabled auth

genAuthUser :: AS.Auth.Auth -> Generator FileDraft
genAuthUser auth =
  return $ mkTmplFdWithData [relfile|server/auth/user.ts|] tmplData
  where
    tmplData =
      object
        [ "userEntityName" .= userEntityName,
          "authEntityName" .= DbAuth.authEntityName,
          "authFieldOnUserEntityName" .= DbAuth.authFieldOnUserEntityName,
          "authIdentityEntityName" .= DbAuth.authIdentityEntityName,
          "identitiesFieldOnAuthEntityName" .= DbAuth.identitiesFieldOnAuthEntityName,
          "enabledProviders" .= AuthProviders.getEnabledAuthProvidersJson auth
        ]
    userEntityName = AS.refName $ AS.Auth.userEntity auth

genHooks :: AS.Auth.Auth -> Generator FileDraft
genHooks auth =
  return $ mkTmplFdWithData [relfile|server/auth/hooks.ts|] tmplData
  where
    tmplData = object ["enabledProviders" .= AuthProviders.getEnabledAuthProvidersJson auth]

genAuthEmail :: AS.Auth.Auth -> Generator [FileDraft]
genAuthEmail auth =
  if AS.Auth.isEmailAuthEnabled auth
    then sequence [return . mkTmplFd $ [relfile|server/auth/email/index.ts|]]
    else return []

genAuthUsername :: AS.Auth.Auth -> Generator [FileDraft]
genAuthUsername auth =
  if AS.Auth.isUsernameAndPasswordAuthEnabled auth
    then sequence [return . mkTmplFd $ [relfile|server/auth/username.ts|]]
    else return []
