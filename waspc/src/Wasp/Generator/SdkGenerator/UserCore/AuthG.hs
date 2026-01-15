module Wasp.Generator.SdkGenerator.UserCore.AuthG
  ( genAuth,
    authDirInUserCoreTemplatesDir,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (Dir', File', Path', Rel, Rel', reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.Core.Ref as AS
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.Common (makeJsArrayFromHaskellList)
import qualified Wasp.Generator.DbGenerator.Auth as DbAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.JsImport (extImportToImportJson)
import Wasp.Generator.SdkGenerator.UserCore.Auth.AuthFormsG (genAuthForms)
import Wasp.Generator.SdkGenerator.UserCore.Auth.EmailAuthG (genEmailAuth)
import Wasp.Generator.SdkGenerator.UserCore.Auth.LocalAuthG (genLocalAuth)
import Wasp.Generator.SdkGenerator.UserCore.Auth.OAuthAuthG (genOAuthAuth)
import Wasp.Generator.SdkGenerator.UserCore.Common
  ( UserCoreTemplatesDir,
    mkTmplFd,
    mkTmplFdWithData,
  )
import Wasp.Generator.SdkGenerator.UserCore.Server.OAuthG (genOAuth)
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
        [ genAuthFileCopy [relfile|user.ts|]
        ]
        -- client stuff
        <++> sequence
          [ genAuthFileCopy [relfile|helpers/user.ts|],
            genAuthFileCopy [relfile|types.ts|],
            genAuthFileCopy [relfile|logout.ts|],
            genUseAuth auth
          ]
        <++> genAuthForms auth
        <++> genLocalAuth auth
        <++> genOAuthAuth auth
        <++> genEmailAuth auth
        -- server stuff
        <++> sequence
          [ return $ mkTmplFd [relfile|core/auth.ts|],
            genAuthFileCopy [relfile|validation.ts|],
            genAuthFileCopy [relfile|jwt.ts|],
            genSessionTs auth,
            genLuciaTs auth,
            genUtils auth,
            genProvidersTypes auth,
            genProvdersIndex auth,
            genIndexTs auth
          ]
        <++> genOAuth auth
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

-- | Generates React hook that Wasp developer can use in a component to get
--   access to the currently logged in user (and check whether user is logged in
--   ot not).
genUseAuth :: AS.Auth.Auth -> Generator FileDraft
genUseAuth auth = return $ mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = authDirInUserCoreTemplatesDir </> [relfile|useAuth.ts|]
    tmplData = object ["entitiesGetMeDependsOn" .= makeJsArrayFromHaskellList [userEntityName]]
    userEntityName = AS.refName $ AS.Auth.userEntity auth

genLuciaTs :: AS.Auth.Auth -> Generator FileDraft
genLuciaTs auth = return $ mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = authDirInUserCoreTemplatesDir </> [relfile|lucia.ts|]
    tmplData =
      object
        [ "sessionEntityLower" .= (Util.toLowerFirst DbAuth.sessionEntityName :: String),
          "authEntityLower" .= (Util.toLowerFirst DbAuth.authEntityName :: String),
          "userEntityUpper" .= (userEntityName :: String)
        ]

    userEntityName = AS.refName $ AS.Auth.userEntity auth

genSessionTs :: AS.Auth.Auth -> Generator FileDraft
genSessionTs auth = return $ mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = authDirInUserCoreTemplatesDir </> [relfile|session.ts|]
    tmplData =
      object
        [ "userEntityUpper" .= userEntityName,
          "userEntityLower" .= Util.toLowerFirst userEntityName,
          "authFieldOnUserEntityName" .= DbAuth.authFieldOnUserEntityName,
          "identitiesFieldOnAuthEntityName" .= DbAuth.identitiesFieldOnAuthEntityName
        ]
    userEntityName = AS.refName $ AS.Auth.userEntity auth

genUtils :: AS.Auth.Auth -> Generator FileDraft
genUtils auth = return $ mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = authDirInUserCoreTemplatesDir </> [relfile|utils.ts|]
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

genIndexTs :: AS.Auth.Auth -> Generator FileDraft
genIndexTs auth = return $ mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = authDirInUserCoreTemplatesDir </> [relfile|index.ts|]
    tmplData =
      object
        [ "isEmailAuthEnabled" .= isEmailAuthEnabled,
          "isLocalAuthEnabled" .= isLocalAuthEnabled
        ]
    isEmailAuthEnabled = AS.Auth.isEmailAuthEnabled auth
    isLocalAuthEnabled = AS.Auth.isUsernameAndPasswordAuthEnabled auth

genProvdersIndex :: AS.Auth.Auth -> Generator FileDraft
genProvdersIndex auth = return $ mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = authDirInUserCoreTemplatesDir </> [relfile|providers/index.ts|]
    tmplData =
      object
        [ "emailUserSignupFields" .= extImportToImportJson userEmailSignupFields,
          "usernameAndPasswordUserSignupFields" .= extImportToImportJson userUsernameAndPassowrdSignupFields
        ]
    userEmailSignupFields = AS.Auth.email authMethods >>= AS.Auth.userSignupFieldsForEmailAuth
    userUsernameAndPassowrdSignupFields = AS.Auth.usernameAndPassword authMethods >>= AS.Auth.userSignupFieldsForUsernameAuth
    authMethods = AS.Auth.methods auth

genProvidersTypes :: AS.Auth.Auth -> Generator FileDraft
genProvidersTypes auth = return $ mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = authDirInUserCoreTemplatesDir </> [relfile|providers/types.ts|]
    tmplData =
      object
        [ "userEntityUpper" .= (userEntityName :: String),
          "emailUserSignupFields" .= extImportToImportJson userEmailSignupFields,
          "usernameAndPasswordUserSignupFields" .= extImportToImportJson userUsernameAndPassowrdSignupFields
        ]
    userEntityName = AS.refName $ AS.Auth.userEntity auth
    userEmailSignupFields = AS.Auth.email authMethods >>= AS.Auth.userSignupFieldsForEmailAuth
    userUsernameAndPassowrdSignupFields = AS.Auth.usernameAndPassword authMethods >>= AS.Auth.userSignupFieldsForUsernameAuth
    authMethods = AS.Auth.methods auth

authDirInUserCoreTemplatesDir :: Path' (Rel UserCoreTemplatesDir) Dir'
authDirInUserCoreTemplatesDir = [reldir|auth|]

genAuthFileCopy :: Path' Rel' File' -> Generator FileDraft
genAuthFileCopy =
  return . mkTmplFd . (authDirInUserCoreTemplatesDir </>)
