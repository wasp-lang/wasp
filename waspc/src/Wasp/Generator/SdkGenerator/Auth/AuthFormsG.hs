module Wasp.Generator.SdkGenerator.Auth.AuthFormsG
  ( genAuthForms,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (reldir, relfile, (</>))
import qualified Wasp.AppSpec.App.Auth as AS.Auth
-- todo(filip) -- Should I put this under something like Wasp.Generator.Auth (doesn't exist) or Wasp.Generator.Common?

import Wasp.AppSpec.App.Auth.AuthMethods (AuthMethod (Email, UsernameAndPassword))
import qualified Wasp.AppSpec.App.Auth.IsEnabled as AS.Auth.IsEnabled
import Wasp.Generator.AuthProviders
  ( gitHubAuthProvider,
    googleAuthProvider,
    keycloakAuthProvider,
  )
import qualified Wasp.Generator.AuthProviders as AuthProviders
import qualified Wasp.Generator.AuthProviders.OAuth as OAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common as C
import Wasp.Generator.WebAppGenerator.Auth.Common (getOnAuthSucceededRedirectToOrDefault)
import Wasp.Util ((<++>))

genAuthForms :: AS.Auth.Auth -> Generator [FileDraft]
genAuthForms auth =
  sequence
    [ genAuthComponent auth,
      genTypes auth,
      -- todo (move this to somewhere more meaningful)
      genFileCopy [relfile|core/stitches.config.ts|],
      genFileCopy [relfile|auth/forms/Login.tsx|],
      genFileCopy [relfile|auth/forms/Signup.tsx|]
    ]
    <++> genEmailForms auth
    <++> genInternalAuthComponents auth
  where
    genFileCopy = return . C.mkTmplFd

genAuthComponent :: AS.Auth.Auth -> Generator FileDraft
genAuthComponent auth =
  return $
    C.mkTmplFdWithData
      [relfile|auth/forms/Auth.tsx|]
      tmplData
  where
    tmplData = object ["isEmailAuthEnabled" .= AS.Auth.IsEnabled.isAuthMethodEnabled Email auth]

genTypes :: AS.Auth.Auth -> Generator FileDraft
genTypes auth =
  return $
    C.mkTmplFdWithData
      [relfile|auth/forms/types.ts|]
      tmplData
  where
    tmplData = object ["isEmailAuthEnabled" .= AS.Auth.IsEnabled.isAuthMethodEnabled Email auth]

genEmailForms :: AS.Auth.Auth -> Generator [FileDraft]
genEmailForms auth =
  genConditionally isEmailAuthEnabled $
    sequence
      [ genFileCopy [relfile|auth/forms/ResetPassword.tsx|],
        genFileCopy [relfile|auth/forms/ForgotPassword.tsx|],
        genFileCopy [relfile|auth/forms/VerifyEmail.tsx|]
      ]
  where
    genFileCopy = return . C.mkTmplFd
    isEmailAuthEnabled = AS.Auth.IsEnabled.isAuthMethodEnabled Email auth

genInternalAuthComponents :: AS.Auth.Auth -> Generator [FileDraft]
genInternalAuthComponents auth =
  sequence
    [ copyInternalAuthComponent [relfile|Form.tsx|],
      copyInternalAuthComponent [relfile|Message.tsx|],
      genLoginSignupForm auth
    ]
    <++> genEmailComponents
    <++> genUsernameAndPasswordComponents
    <++> genSocialComponents
  where
    genEmailComponents =
      genConditionally isEmailAuthEnabled $
        sequence
          [ copyInternalAuthComponent [relfile|email/VerifyEmailForm.tsx|],
            copyInternalAuthComponent [relfile|email/useEmail.ts|],
            copyInternalAuthComponent [relfile|email/ForgotPasswordForm.tsx|],
            copyInternalAuthComponent [relfile|email/ResetPasswordForm.tsx|]
          ]
    genUsernameAndPasswordComponents =
      genConditionally isUsernameAndPasswordAuthEnabled $
        sequence
          [ copyInternalAuthComponent [relfile|usernameAndPassword/useUsernameAndPassword.ts|]
          ]
    genSocialComponents =
      genConditionally isExternalAuthEnabled $
        sequence
          [ copyInternalAuthComponent [relfile|social/SocialButton.tsx|],
            copyInternalAuthComponent [relfile|social/SocialIcons.tsx|]
          ]

    isExternalAuthEnabled = AS.Auth.IsEnabled.isExternalAuthEnabled auth
    isUsernameAndPasswordAuthEnabled = AS.Auth.IsEnabled.isAuthMethodEnabled UsernameAndPassword auth
    isEmailAuthEnabled = AS.Auth.IsEnabled.isAuthMethodEnabled Email auth

    copyInternalAuthComponent = return . C.mkTmplFd . (pathToInternalInAuth </>)
    pathToInternalInAuth = [reldir|auth/forms/internal|]

genLoginSignupForm :: AS.Auth.Auth -> Generator FileDraft
genLoginSignupForm auth =
  return $
    C.mkTmplFdWithData
      [relfile|auth/forms/internal/common/LoginSignupForm.tsx|]
      tmplData
  where
    tmplData =
      object
        [ "onAuthSucceededRedirectTo" .= getOnAuthSucceededRedirectToOrDefault auth,
          "areBothSocialAndPasswordBasedAuthEnabled" .= areBothSocialAndPasswordBasedAuthEnabled,
          "isAnyPasswordBasedAuthEnabled" .= isAnyPasswordBasedAuthEnabled,
          "isSocialAuthEnabled" .= AS.Auth.IsEnabled.isExternalAuthEnabled auth,
          "providers" .= AuthProviders.getAuthProvidersJson auth,
          "googleSignInPath" .= OAuth.serverLoginUrl googleAuthProvider,
          "keycloakSignInPath" .= OAuth.serverLoginUrl keycloakAuthProvider,
          "gitHubSignInPath" .= OAuth.serverLoginUrl gitHubAuthProvider
        ]
    areBothSocialAndPasswordBasedAuthEnabled = AS.Auth.IsEnabled.isExternalAuthEnabled auth && isAnyPasswordBasedAuthEnabled
    isAnyPasswordBasedAuthEnabled = AS.Auth.IsEnabled.isAuthMethodEnabled UsernameAndPassword auth || AS.Auth.IsEnabled.isAuthMethodEnabled Email auth

genConditionally :: Bool -> Generator [FileDraft] -> Generator [FileDraft]
genConditionally isEnabled gen = if isEnabled then gen else return []
