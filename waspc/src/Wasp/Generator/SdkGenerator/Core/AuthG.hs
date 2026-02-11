{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Generator.SdkGenerator.Core.AuthG
  ( genAuth,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.AuthProviders (emailAuthProvider, localAuthProvider)
import qualified Wasp.Generator.AuthProviders.Email as AP.Email
import qualified Wasp.Generator.AuthProviders.Local as AP.UsernameAndPassword
import Wasp.Generator.Common (genConditionally)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Core.Common (mkTmplFd, mkTmplFdWithData)
import Wasp.Util ((<++>))

genAuth :: AppSpec -> Generator [FileDraft]
genAuth spec =
  case maybeAuth of
    Nothing -> return []
    Just auth ->
      return
        [ mkTmplFd [relfile|auth/logout.ts|],
          mkTmplFd [relfile|auth/helpers/user.ts|],
          mkTmplFd [relfile|auth/password.ts|],
          mkTmplFd [relfile|auth/validation.ts|],
          mkTmplFd [relfile|auth/forms/internal/util.ts|],
          mkTmplFd [relfile|auth/forms/internal/auth-styles.css|],
          mkTmplFd [relfile|auth/forms/internal/Form.tsx|],
          mkTmplFd [relfile|auth/forms/internal/Form.module.css|],
          mkTmplFd [relfile|auth/forms/internal/Message.tsx|],
          mkTmplFd [relfile|auth/forms/internal/Message.module.css|],
          mkTmplFd [relfile|auth/forms/internal/email/VerifyEmailForm.tsx|],
          mkTmplFd [relfile|auth/forms/internal/email/ForgotPasswordForm.tsx|],
          mkTmplFd [relfile|auth/forms/internal/email/ResetPasswordForm.tsx|]
        ]
        <++> genSocialComponents auth
        <++> genAuthEmailActions
        <++> genAuthUsernameAndPasswordActions
        <++> genAuthFormsTypes auth
  where
    maybeAuth = (snd $ getApp spec).auth

genAuthFormsTypes :: AS.Auth.Auth -> Generator [FileDraft]
genAuthFormsTypes auth =
  return [mkTmplFdWithData [relfile|auth/forms/types.ts|] tmplData]
  where
    tmplData = object ["isEmailAuthEnabled" .= AS.Auth.isEmailAuthEnabled auth]

genSocialComponents :: AS.Auth.Auth -> Generator [FileDraft]
genSocialComponents auth =
  genConditionally
    isExternalAuthEnabled
    [ mkTmplFd [relfile|auth/forms/internal/social/SocialButton.tsx|],
      mkTmplFd [relfile|auth/forms/internal/social/SocialButton.module.css|],
      mkTmplFd [relfile|auth/forms/internal/social/SocialIcons.tsx|],
      mkTmplFd [relfile|auth/forms/internal/social/SocialIcons.module.css|]
    ]
  where
    isExternalAuthEnabled = AS.Auth.isExternalAuthEnabled auth

genAuthUsernameAndPasswordActions :: Generator [FileDraft]
genAuthUsernameAndPasswordActions =
  sequence
    [ genLoginAction
    ]
  where
    genLoginAction :: Generator FileDraft
    genLoginAction =
      return $ mkTmplFdWithData [relfile|auth/username/actions/login.ts|] tmplData
      where
        tmplData = object ["loginPath" .= AP.UsernameAndPassword.serverLoginUrl localAuthProvider]

genAuthEmailActions :: Generator [FileDraft]
genAuthEmailActions =
  sequence
    [ genLoginAction,
      genPasswordResetActions,
      genVerifyEmailAction
    ]
  where
    genLoginAction :: Generator FileDraft
    genLoginAction =
      return $ mkTmplFdWithData [relfile|auth/email/actions/login.ts|] tmplData
      where
        tmplData = object ["loginPath" .= AP.Email.serverLoginUrl emailAuthProvider]

    genPasswordResetActions :: Generator FileDraft
    genPasswordResetActions =
      return $ mkTmplFdWithData [relfile|auth/email/actions/passwordReset.ts|] tmplData
      where
        tmplData =
          object
            [ "requestPasswordResetPath" .= AP.Email.serverRequestPasswordResetUrl emailAuthProvider,
              "resetPasswordPath" .= AP.Email.serverResetPasswordUrl emailAuthProvider
            ]

    genVerifyEmailAction :: Generator FileDraft
    genVerifyEmailAction =
      return $ mkTmplFdWithData [relfile|auth/email/actions/verifyEmail.ts|] tmplData
      where
        tmplData = object ["verifyEmailPath" .= AP.Email.serverVerifyEmailUrl emailAuthProvider]
