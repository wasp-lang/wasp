module Wasp.Generator.SdkGenerator.Auth.EmailAuthG
  ( genEmailAuth,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (relfile)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.AuthProviders (emailAuthProvider)
import Wasp.Generator.AuthProviders.Email
  ( serverLoginUrl,
    serverRequestPasswordResetUrl,
    serverResetPasswordUrl,
    serverSignupUrl,
    serverVerifyEmailUrl,
  )
import qualified Wasp.Generator.DbGenerator.Auth as DbAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common as C
import Wasp.Generator.SdkGenerator.JsImport (extImportToImportJson)
import Wasp.Util ((<++>))
import qualified Wasp.Util as Util

genEmailAuth :: AS.Auth.Auth -> Generator [FileDraft]
genEmailAuth auth
  | AS.Auth.isEmailAuthEnabled auth =
      sequence
        [ genIndex,
          genServerUtils auth
        ]
        <++> genActions auth
  | otherwise = return []

genIndex :: Generator FileDraft
genIndex = return $ C.mkTmplFd [relfile|auth/email/index.ts|]

genServerUtils :: AS.Auth.Auth -> Generator FileDraft
genServerUtils auth = return $ C.mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = [relfile|server/auth/email/utils.ts|]
    tmplData =
      object
        [ "userEntityUpper" .= (userEntityName :: String),
          "userEntityLower" .= (Util.toLowerFirst userEntityName :: String),
          "authEntityUpper" .= (DbAuth.authEntityName :: String),
          "authEntityLower" .= (Util.toLowerFirst DbAuth.authEntityName :: String),
          "userFieldOnAuthEntityName" .= (DbAuth.userFieldOnAuthEntityName :: String)
        ]
    userEntityName = AS.refName $ AS.Auth.userEntity auth

genActions :: AS.Auth.Auth -> Generator [FileDraft]
genActions auth =
  sequence
    [ genLoginAction,
      genSignupAction auth,
      genPasswordResetActions,
      genVerifyEmailAction
    ]

genLoginAction :: Generator FileDraft
genLoginAction =
  return $ C.mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = [relfile|auth/email/actions/login.ts|]
    tmplData = object ["loginPath" .= serverLoginUrl emailAuthProvider]

genSignupAction :: AS.Auth.Auth -> Generator FileDraft
genSignupAction auth =
  return $ C.mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = [relfile|auth/email/actions/signup.ts|]
    tmplData =
      object
        [ "signupPath" .= serverSignupUrl emailAuthProvider,
          "emailUserSignupFields" .= extImportToImportJson userEmailSignupFields
        ]
    userEmailSignupFields = AS.Auth.email authMethods >>= AS.Auth.userSignupFieldsForEmailAuth
    authMethods = AS.Auth.methods auth

genPasswordResetActions :: Generator FileDraft
genPasswordResetActions =
  return $ C.mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = [relfile|auth/email/actions/passwordReset.ts|]
    tmplData =
      object
        [ "requestPasswordResetPath" .= serverRequestPasswordResetUrl emailAuthProvider,
          "resetPasswordPath" .= serverResetPasswordUrl emailAuthProvider
        ]

genVerifyEmailAction :: Generator FileDraft
genVerifyEmailAction =
  return $ C.mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = [relfile|auth/email/actions/verifyEmail.ts|]
    tmplData = object ["verifyEmailPath" .= serverVerifyEmailUrl emailAuthProvider]
