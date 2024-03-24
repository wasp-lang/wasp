module Wasp.Generator.SdkGenerator.AuthG
  ( genAuth,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (File', Path', Rel, relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.Generator.AuthProviders as AuthProviders
import Wasp.Generator.Common (makeJsArrayFromHaskellList)
import qualified Wasp.Generator.DbGenerator.Auth as DbAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Auth.AuthFormsG (genAuthForms)
import Wasp.Generator.SdkGenerator.Auth.EmailAuthG (genEmailAuth)
import Wasp.Generator.SdkGenerator.Auth.LocalAuthG (genLocalAuth)
import Wasp.Generator.SdkGenerator.Auth.OAuthAuthG (genOAuthAuth)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Generator.WebAppGenerator.Auth.Common (getOnAuthSucceededRedirectToOrDefault)
import Wasp.Util ((<++>))
import qualified Wasp.Util as Util

genAuth :: AppSpec -> Generator [FileDraft]
genAuth spec =
  case maybeAuth of
    Nothing -> return []
    Just auth ->
      -- shared stuff
      sequence
        [ genUserTs auth
        ]
        -- client stuff
        <++> sequence
          [ genFileCopy [relfile|auth/helpers/user.ts|],
            genFileCopy [relfile|auth/types.ts|],
            genFileCopy [relfile|auth/logout.ts|],
            genUseAuth auth
          ]
        <++> genAuthForms auth
        <++> genLocalAuth auth
        <++> genOAuthAuth auth
        <++> genEmailAuth auth
        -- server stuff
        <++> sequence
          [ genFileCopy [relfile|core/auth.ts|],
            genFileCopy [relfile|auth/validation.ts|],
            genFileCopy [relfile|auth/password.ts|],
            genFileCopy [relfile|auth/jwt.ts|],
            genSessionTs auth,
            genLuciaTs auth,
            genUtils auth,
            genProvidersTypes auth
          ]
        <++> genIndexTs auth
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec
    genFileCopy = return . C.mkTmplFd

-- | Generates React hook that Wasp developer can use in a component to get
--   access to the currently logged in user (and check whether user is logged in
--   ot not).
genUseAuth :: AS.Auth.Auth -> Generator FileDraft
genUseAuth auth = return $ C.mkTmplFdWithData [relfile|auth/useAuth.ts|] tmplData
  where
    tmplData = object ["entitiesGetMeDependsOn" .= makeJsArrayFromHaskellList [userEntityName]]
    userEntityName = AS.refName $ AS.Auth.userEntity auth

genLuciaTs :: AS.Auth.Auth -> Generator FileDraft
genLuciaTs auth = return $ C.mkTmplFdWithData [relfile|auth/lucia.ts|] tmplData
  where
    tmplData =
      object
        [ "sessionEntityLower" .= (Util.toLowerFirst DbAuth.sessionEntityName :: String),
          "authEntityLower" .= (Util.toLowerFirst DbAuth.authEntityName :: String),
          "userEntityUpper" .= (userEntityName :: String)
        ]

    userEntityName = AS.refName $ AS.Auth.userEntity auth

genSessionTs :: AS.Auth.Auth -> Generator FileDraft
genSessionTs auth = return $ C.mkTmplFdWithData [relfile|auth/session.ts|] tmplData
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
genUtils auth = return $ C.mkTmplFdWithData relUtilsFilePath tmplData
  where
    userEntityName = AS.refName $ AS.Auth.userEntity auth
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

    relUtilsFilePath :: Path' (Rel C.SdkTemplatesDir) File'
    relUtilsFilePath = [relfile|auth/utils.ts|]

genIndexTs :: AS.Auth.Auth -> Generator [FileDraft]
genIndexTs auth =
  return [C.mkTmplFdWithData [relfile|auth/index.ts|] tmplData]
  where
    tmplData =
      object
        [ "isEmailAuthEnabled" .= isEmailAuthEnabled,
          "isLocalAuthEnabled" .= isLocalAuthEnabled
        ]
    isEmailAuthEnabled = AS.Auth.isEmailAuthEnabled auth
    isLocalAuthEnabled = AS.Auth.isUsernameAndPasswordAuthEnabled auth

genProvidersTypes :: AS.Auth.Auth -> Generator FileDraft
genProvidersTypes auth = return $ C.mkTmplFdWithData [relfile|auth/providers/types.ts|] tmplData
  where
    userEntityName = AS.refName $ AS.Auth.userEntity auth

    tmplData = object ["userEntityUpper" .= (userEntityName :: String)]

genUserTs :: AS.Auth.Auth -> Generator FileDraft
genUserTs auth = return $ C.mkTmplFdWithData [relfile|auth/user.ts|] tmplData
  where
    userEntityName = AS.refName $ AS.Auth.userEntity auth

    tmplData =
      object
        [ "userEntityName" .= userEntityName,
          "authEntityName" .= DbAuth.authEntityName,
          "authFieldOnUserEntityName" .= DbAuth.authFieldOnUserEntityName,
          "authIdentityEntityName" .= DbAuth.authIdentityEntityName,
          "identitiesFieldOnAuthEntityName" .= DbAuth.identitiesFieldOnAuthEntityName,
          "enabledProviders" .= AuthProviders.getEnabledAuthProvidersJson auth
        ]
