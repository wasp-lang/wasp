module Wasp.Generator.SdkGenerator.Auth.AuthFormsG
  ( genAuthForms,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (reldir, relfile, (</>))
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.EmailSender as AS.EmailSender
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
-- todo(filip) -- Should I put this under something like Wasp.Generator.Auth (doesn't exist) or Wasp.Generator.Common?
import Wasp.Generator.WebAppGenerator.Auth.Common (getOnAuthSucceededRedirectToOrDefault)
import Wasp.Util ((<++>))

genAuthForms :: AS.App.App -> AS.Auth.Auth -> Generator [FileDraft]
genAuthForms app auth =
  sequence
    [ genAuthComponent auth,
      genTypes auth,
      -- TODO: Move Stitches to somewhere more meaningful.
      -- Currently, they are used only when auth is used,
      -- but that might change. Also, it feels wrong to
      -- generate general styling config in auth generator.
      genFileCopy [relfile|core/stitches.config.ts|],
      genFileCopy [relfile|auth/forms/Login.tsx|],
      genFileCopy [relfile|auth/forms/Signup.tsx|]
    ]
    <++> genEmailForms auth
    <++> genInternalAuthComponents app auth
  where
    genFileCopy = return . C.mkTmplFd

genAuthComponent :: AS.Auth.Auth -> Generator FileDraft
genAuthComponent auth =
  return $
    C.mkTmplFdWithData
      [relfile|auth/forms/Auth.tsx|]
      tmplData
  where
    tmplData = object ["isEmailAuthEnabled" .= AS.Auth.isEmailAuthEnabled auth]

genTypes :: AS.Auth.Auth -> Generator FileDraft
genTypes auth =
  return $
    C.mkTmplFdWithData
      [relfile|auth/forms/types.ts|]
      tmplData
  where
    tmplData = object ["isEmailAuthEnabled" .= AS.Auth.isEmailAuthEnabled auth]

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
    isEmailAuthEnabled = AS.Auth.isEmailAuthEnabled auth

genInternalAuthComponents :: AS.App.App -> AS.Auth.Auth -> Generator [FileDraft]
genInternalAuthComponents app auth =
  sequence
    [ copyInternalAuthComponent [relfile|Form.tsx|],
      copyInternalAuthComponent [relfile|Message.tsx|],
      genLoginSignupForm app auth
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

    isExternalAuthEnabled = AS.Auth.isExternalAuthEnabled auth
    isUsernameAndPasswordAuthEnabled = AS.Auth.isUsernameAndPasswordAuthEnabled auth
    isEmailAuthEnabled = AS.Auth.isEmailAuthEnabled auth

    copyInternalAuthComponent = return . C.mkTmplFd . (pathToInternalInAuth </>)
    pathToInternalInAuth = [reldir|auth/forms/internal|]

genLoginSignupForm :: AS.App.App -> AS.Auth.Auth -> Generator FileDraft
genLoginSignupForm app auth =
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
          "isSocialAuthEnabled" .= AS.Auth.isExternalAuthEnabled auth,
          "googleSignInPath" .= OAuth.serverLoginUrl googleAuthProvider,
          "keycloakSignInPath" .= OAuth.serverLoginUrl keycloakAuthProvider,
          "gitHubSignInPath" .= OAuth.serverLoginUrl gitHubAuthProvider,
          "enabledProviders" .= AuthProviders.getEnabledAuthProvidersJson auth,
          "isEmailSenderProviderSetToDummy" .= isEmailSenderProviderSetToDummy
        ]
    areBothSocialAndPasswordBasedAuthEnabled = AS.Auth.isExternalAuthEnabled auth && isAnyPasswordBasedAuthEnabled
    isAnyPasswordBasedAuthEnabled = AS.Auth.isUsernameAndPasswordAuthEnabled auth || AS.Auth.isEmailAuthEnabled auth
    isEmailSenderProviderSetToDummy = (AS.EmailSender.provider <$> AS.App.emailSender app) == Just AS.EmailSender.Dummy

genConditionally :: Bool -> Generator [FileDraft] -> Generator [FileDraft]
genConditionally isEnabled gen = if isEnabled then gen else return []
