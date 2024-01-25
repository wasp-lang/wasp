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
import Wasp.Util ((<++>))
import qualified Wasp.Util as Util

genEmailAuth :: AS.Auth.Auth -> Generator [FileDraft]
genEmailAuth auth
  | AS.Auth.isEmailAuthEnabled auth =
      sequence
        [ genIndex,
          genServerUtils auth
        ]
        <++> genActions
  | otherwise = return []

genIndex :: Generator FileDraft
genIndex = return $ C.mkTmplFd [relfile|auth/email/index.ts|]

genActions :: Generator [FileDraft]
genActions =
  sequence
    [ genLoginAction,
      genSignupAction,
      genPasswordResetActions,
      genVerifyEmailAction
    ]

genLoginAction :: Generator FileDraft
genLoginAction =
  return $
    C.mkTmplFdWithData
      [relfile|auth/email/actions/login.ts|]
      (object ["loginPath" .= serverLoginUrl emailAuthProvider])

genSignupAction :: Generator FileDraft
genSignupAction =
  return $
    C.mkTmplFdWithData
      [relfile|auth/email/actions/signup.ts|]
      (object ["signupPath" .= serverSignupUrl emailAuthProvider])

genPasswordResetActions :: Generator FileDraft
genPasswordResetActions =
  return $
    C.mkTmplFdWithData
      [relfile|auth/email/actions/passwordReset.ts|]
      ( object
          [ "requestPasswordResetPath" .= serverRequestPasswordResetUrl emailAuthProvider,
            "resetPasswordPath" .= serverResetPasswordUrl emailAuthProvider
          ]
      )

genVerifyEmailAction :: Generator FileDraft
genVerifyEmailAction =
  return $
    C.mkTmplFdWithData
      [relfile|auth/email/actions/verifyEmail.ts|]
      (object ["verifyEmailPath" .= serverVerifyEmailUrl emailAuthProvider])

genServerUtils :: AS.Auth.Auth -> Generator FileDraft
genServerUtils auth = return $ C.mkTmplFdWithData tmplFile tmplData
  where
    userEntityName = AS.refName $ AS.Auth.userEntity auth
    tmplFile = [relfile|server/auth/email/utils.ts|]
    tmplData =
      object
        [ "userEntityUpper" .= (userEntityName :: String),
          "userEntityLower" .= (Util.toLowerFirst userEntityName :: String),
          "authEntityUpper" .= (DbAuth.authEntityName :: String),
          "authEntityLower" .= (Util.toLowerFirst DbAuth.authEntityName :: String),
          "userFieldOnAuthEntityName" .= (DbAuth.userFieldOnAuthEntityName :: String)
        ]
