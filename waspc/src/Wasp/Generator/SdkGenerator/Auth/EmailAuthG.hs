module Wasp.Generator.SdkGenerator.Auth.EmailAuthG
  ( genEmailAuth,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (isJust)
import StrongPath (Dir', File', Path', Rel, Rel', reldir, relfile, (</>))
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
import Wasp.Generator.SdkGenerator.Common
  ( SdkTemplatesDir,
    genFileCopy,
    mkTmplFdWithData,
  )
import Wasp.Util ((<++>))

genEmailAuth :: AS.Auth.Auth -> Generator [FileDraft]
genEmailAuth auth
  | AS.Auth.isEmailAuthEnabled auth =
      sequence
        [ genFileCopyInEmailAuthDir [relfile|index.ts|]
        ]
        <++> genActions auth
  | otherwise = return []

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
  return $
    mkTmplFdWithData
      (emailAuthDirInSdkTemplatesDir </> [relfile|actions/login.ts|])
      tmplData
  where
    tmplData = object ["loginPath" .= serverLoginUrl emailAuthProvider]

genSignupAction :: AS.Auth.Auth -> Generator FileDraft
genSignupAction auth =
  return $
    mkTmplFdWithData
      (emailAuthDirInSdkTemplatesDir </> [relfile|actions/signup.ts|])
      tmplData
  where
    tmplData =
      object
        [ "signupPath" .= serverSignupUrl emailAuthProvider,
          "isEmailUserSignupFieldsDefined" .= isJust emailUserSignupFields
        ]
    emailUserSignupFields = AS.Auth.email authMethods >>= AS.Auth.userSignupFieldsForEmailAuth
    authMethods = AS.Auth.methods auth

genPasswordResetActions :: Generator FileDraft
genPasswordResetActions =
  return $
    mkTmplFdWithData
      (emailAuthDirInSdkTemplatesDir </> [relfile|actions/passwordReset.ts|])
      tmplData
  where
    tmplData =
      object
        [ "requestPasswordResetPath" .= serverRequestPasswordResetUrl emailAuthProvider,
          "resetPasswordPath" .= serverResetPasswordUrl emailAuthProvider
        ]

genVerifyEmailAction :: Generator FileDraft
genVerifyEmailAction =
  return $
    mkTmplFdWithData
      (emailAuthDirInSdkTemplatesDir </> [relfile|actions/verifyEmail.ts|])
      tmplData
  where
    tmplData = object ["verifyEmailPath" .= serverVerifyEmailUrl emailAuthProvider]

emailAuthDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) Dir'
emailAuthDirInSdkTemplatesDir = [reldir|auth/email|]

genFileCopyInEmailAuthDir :: Path' Rel' File' -> Generator FileDraft
genFileCopyInEmailAuthDir =
  genFileCopy . (emailAuthDirInSdkTemplatesDir </>)
