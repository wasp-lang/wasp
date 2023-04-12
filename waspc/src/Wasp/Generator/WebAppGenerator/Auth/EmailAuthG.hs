module Wasp.Generator.WebAppGenerator.Auth.EmailAuthG
  ( genEmailAuth,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (relfile)
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.AuthProviders (emailAuthProvider)
import Wasp.Generator.AuthProviders.Email
  ( serverLoginUrl,
    serverRequestPasswordResetUrl,
    serverResetPasswordUrl,
    serverSignupUrl,
    serverVerifyEmailUrl,
  )
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Util ((<++>))

genEmailAuth :: AS.Auth.Auth -> Generator [FileDraft]
genEmailAuth auth
  | AS.Auth.isEmailAuthEnabled auth =
      sequence
        [ genIndex
        ]
        <++> genActions
  | otherwise = return []

genIndex :: Generator FileDraft
genIndex = return $ C.mkSrcTmplFd [relfile|auth/email/index.ts|]

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
      [relfile|src/auth/email/actions/login.ts|]
      (object ["loginPath" .= serverLoginUrl emailAuthProvider])

genSignupAction :: Generator FileDraft
genSignupAction =
  return $
    C.mkTmplFdWithData
      [relfile|src/auth/email/actions/signup.ts|]
      (object ["signupPath" .= serverSignupUrl emailAuthProvider])

genPasswordResetActions :: Generator FileDraft
genPasswordResetActions =
  return $
    C.mkTmplFdWithData
      [relfile|src/auth/email/actions/passwordReset.ts|]
      ( object
          [ "requestPasswordResetPath" .= serverRequestPasswordResetUrl emailAuthProvider,
            "resetPasswordPath" .= serverResetPasswordUrl emailAuthProvider
          ]
      )

genVerifyEmailAction :: Generator FileDraft
genVerifyEmailAction =
  return $
    C.mkTmplFdWithData
      [relfile|src/auth/email/actions/verifyEmail.ts|]
      (object ["verifyEmailPath" .= serverVerifyEmailUrl emailAuthProvider])
