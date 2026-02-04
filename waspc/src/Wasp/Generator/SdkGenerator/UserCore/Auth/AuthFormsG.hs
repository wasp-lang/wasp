module Wasp.Generator.SdkGenerator.UserCore.Auth.AuthFormsG
  ( genAuthForms,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (relfile)
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.AuthProviders
  ( discordAuthProvider,
    gitHubAuthProvider,
    googleAuthProvider,
    keycloakAuthProvider,
    slackAuthProvider,
  )
import qualified Wasp.Generator.AuthProviders as AuthProviders
import qualified Wasp.Generator.AuthProviders.OAuth as OAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.UserCore.Common
  ( mkTmplFd,
    mkTmplFdWithData,
  )
-- todo(filip) -- Should I put this under something like Wasp.Generator.Auth (doesn't exist) or Wasp.Generator.Common?
import Wasp.Generator.WebAppGenerator.Auth.Common (getOnAuthSucceededRedirectToOrDefault)
import Wasp.Util ((<++>))

genAuthForms :: AS.Auth.Auth -> Generator [FileDraft]
genAuthForms auth =
  sequence
    [ genAuthComponent auth,
      return . mkTmplFd $ [relfile|auth/forms/Auth.module.css|],
      genTypes auth,
      return . mkTmplFd $ [relfile|auth/forms/Login.tsx|],
      return . mkTmplFd $ [relfile|auth/forms/Signup.tsx|]
    ]
    <++> genEmailForms auth
    <++> genInternalAuthComponents auth

genAuthComponent :: AS.Auth.Auth -> Generator FileDraft
genAuthComponent auth =
  return $ mkTmplFdWithData [relfile|auth/forms/Auth.tsx|] tmplData
  where
    tmplData = object ["isEmailAuthEnabled" .= AS.Auth.isEmailAuthEnabled auth]

genTypes :: AS.Auth.Auth -> Generator FileDraft
genTypes auth =
  return $ mkTmplFdWithData [relfile|auth/forms/types.ts|] tmplData
  where
    tmplData = object ["isEmailAuthEnabled" .= AS.Auth.isEmailAuthEnabled auth]

genEmailForms :: AS.Auth.Auth -> Generator [FileDraft]
genEmailForms auth =
  genConditionally isEmailAuthEnabled $
    sequence
      [ return . mkTmplFd $ [relfile|auth/forms/ResetPassword.tsx|],
        return . mkTmplFd $ [relfile|auth/forms/ForgotPassword.tsx|],
        return . mkTmplFd $ [relfile|auth/forms/VerifyEmail.tsx|]
      ]
  where
    isEmailAuthEnabled = AS.Auth.isEmailAuthEnabled auth

genInternalAuthComponents :: AS.Auth.Auth -> Generator [FileDraft]
genInternalAuthComponents auth =
  sequence
    [ return . mkTmplFd $ [relfile|auth/forms/internal/auth-styles.css|],
      return . mkTmplFd $ [relfile|auth/forms/internal/util.ts|]
    ]
    <++> genLoginSignupForm auth
    <++> genFormComponent
    <++> genMessageComponent
    <++> genEmailComponents
    <++> genUsernameAndPasswordComponents
    <++> genSocialComponents
  where
    genFormComponent =
      sequence
        [ return . mkTmplFd $ [relfile|auth/forms/internal/Form.tsx|],
          return . mkTmplFd $ [relfile|auth/forms/internal/Form.module.css|]
        ]

    genMessageComponent =
      sequence
        [ return . mkTmplFd $ [relfile|auth/forms/internal/Message.tsx|],
          return . mkTmplFd $ [relfile|auth/forms/internal/Message.module.css|]
        ]

    genEmailComponents =
      genConditionally isEmailAuthEnabled $
        sequence
          [ return . mkTmplFd $ [relfile|auth/forms/internal/email/VerifyEmailForm.tsx|],
            return . mkTmplFd $ [relfile|auth/forms/internal/email/useEmail.ts|],
            return . mkTmplFd $ [relfile|auth/forms/internal/email/ForgotPasswordForm.tsx|],
            return . mkTmplFd $ [relfile|auth/forms/internal/email/ResetPasswordForm.tsx|]
          ]

    genUsernameAndPasswordComponents =
      genConditionally isUsernameAndPasswordAuthEnabled $
        sequence
          [ return . mkTmplFd $ [relfile|auth/forms/internal/usernameAndPassword/useUsernameAndPassword.ts|]
          ]

    genSocialComponents =
      genConditionally isExternalAuthEnabled $
        genSocialButtonComponent
          <++> genSocialIconsComponent

    genSocialButtonComponent =
      sequence
        [ return . mkTmplFd $ [relfile|auth/forms/internal/social/SocialButton.tsx|],
          return . mkTmplFd $ [relfile|auth/forms/internal/social/SocialButton.module.css|]
        ]

    genSocialIconsComponent =
      sequence
        [ return . mkTmplFd $ [relfile|auth/forms/internal/social/SocialIcons.tsx|],
          return . mkTmplFd $ [relfile|auth/forms/internal/social/SocialIcons.module.css|]
        ]

    isEmailAuthEnabled = AS.Auth.isEmailAuthEnabled auth
    isUsernameAndPasswordAuthEnabled = AS.Auth.isUsernameAndPasswordAuthEnabled auth
    isExternalAuthEnabled = AS.Auth.isExternalAuthEnabled auth

genLoginSignupForm :: AS.Auth.Auth -> Generator [FileDraft]
genLoginSignupForm auth =
  sequence
    [ genLoginSigunFormComponent auth,
      return . mkTmplFd $ [relfile|auth/forms/internal/common/LoginSignupForm.module.css|]
    ]

genLoginSigunFormComponent :: AS.Auth.Auth -> Generator FileDraft
genLoginSigunFormComponent auth =
  return $ mkTmplFdWithData [relfile|auth/forms/internal/common/LoginSignupForm.tsx|] tmplData
  where
    tmplData =
      object
        [ "onAuthSucceededRedirectTo" .= getOnAuthSucceededRedirectToOrDefault auth,
          "areBothSocialAndPasswordBasedAuthEnabled" .= areBothSocialAndPasswordBasedAuthEnabled,
          "isAnyPasswordBasedAuthEnabled" .= isAnyPasswordBasedAuthEnabled,
          "isSocialAuthEnabled" .= AS.Auth.isExternalAuthEnabled auth,
          "slackSignInPath" .= OAuth.serverLoginUrl slackAuthProvider,
          "discordSignInPath" .= OAuth.serverLoginUrl discordAuthProvider,
          "googleSignInPath" .= OAuth.serverLoginUrl googleAuthProvider,
          "keycloakSignInPath" .= OAuth.serverLoginUrl keycloakAuthProvider,
          "gitHubSignInPath" .= OAuth.serverLoginUrl gitHubAuthProvider,
          "enabledProviders" .= AuthProviders.getEnabledAuthProvidersJson auth
        ]
    areBothSocialAndPasswordBasedAuthEnabled = AS.Auth.isExternalAuthEnabled auth && isAnyPasswordBasedAuthEnabled
    isAnyPasswordBasedAuthEnabled = AS.Auth.isUsernameAndPasswordAuthEnabled auth || AS.Auth.isEmailAuthEnabled auth

genConditionally :: Bool -> Generator [FileDraft] -> Generator [FileDraft]
genConditionally isEnabled gen = if isEnabled then gen else return []
