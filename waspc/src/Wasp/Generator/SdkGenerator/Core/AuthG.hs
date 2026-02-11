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
import Wasp.Generator.AuthProviders (emailAuthProvider)
import Wasp.Generator.AuthProviders.Email
  ( serverLoginUrl,
    serverRequestPasswordResetUrl,
    serverResetPasswordUrl,
    serverVerifyEmailUrl,
  )
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
          mkTmplFd [relfile|auth/forms/internal/Message.module.css|]
        ]
        <++> genSocialComponents auth
        <++> genAuthEmailActions
  where
    maybeAuth = (snd $ getApp spec).auth

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

genAuthEmailActions :: Generator [FileDraft]
genAuthEmailActions =
  sequence
    [ genLoginAction,
      genPasswordResetActions,
      genVerifyEmailAction
    ]

genLoginAction :: Generator FileDraft
genLoginAction =
  return $ mkTmplFdWithData [relfile|auth/email/actions/login.ts|] tmplData
  where
    tmplData = object ["loginPath" .= serverLoginUrl emailAuthProvider]

genPasswordResetActions :: Generator FileDraft
genPasswordResetActions =
  return $ mkTmplFdWithData [relfile|auth/email/actions/passwordReset.ts|] tmplData
  where
    tmplData =
      object
        [ "requestPasswordResetPath" .= serverRequestPasswordResetUrl emailAuthProvider,
          "resetPasswordPath" .= serverResetPasswordUrl emailAuthProvider
        ]

genVerifyEmailAction :: Generator FileDraft
genVerifyEmailAction =
  return $ mkTmplFdWithData [relfile|auth/email/actions/verifyEmail.ts|] tmplData
  where
    tmplData = object ["verifyEmailPath" .= serverVerifyEmailUrl emailAuthProvider]
