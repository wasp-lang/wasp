module Wasp.Generator.SdkGenerator.Auth.AuthFormsG
  ( genAuthForms,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (reldir, relfile, (</>))
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
import Wasp.Generator.SdkGenerator.Common as C
-- todo(filip) -- Should I put this under something like Wasp.Generator.Auth (doesn't exist) or Wasp.Generator.Common?
import Wasp.Generator.WebAppGenerator.Auth.Common (getOnAuthSucceededRedirectToOrDefault)
import Wasp.Util ((<++>))

genAuthForms :: AS.Auth.Auth -> Generator [FileDraft]
genAuthForms auth =
  sequence
    [ genAuthComponent auth,
      genFileCopy [relfile|auth/forms/Auth.module.css|],
      genTypes auth,
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

genInternalAuthComponents :: AS.Auth.Auth -> Generator [FileDraft]
genInternalAuthComponents auth =
  sequence
    [ copyInternalAuthComponent [relfile|auth-styles.css|],
      copyInternalAuthComponent [relfile|util.ts|]
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
        [ copyInternalAuthComponent [relfile|Form.tsx|],
          copyInternalAuthComponent [relfile|Form.module.css|]
        ]

    genMessageComponent =
      sequence
        [ copyInternalAuthComponent [relfile|Message.tsx|],
          copyInternalAuthComponent [relfile|Message.module.css|]
        ]

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
        genSocialButtonComponent
          <++> genSocialIconsComponent

    genSocialButtonComponent =
      sequence
        [ copyInternalAuthComponent [relfile|social/SocialButton.tsx|],
          copyInternalAuthComponent [relfile|social/SocialButton.module.css|]
        ]

    genSocialIconsComponent =
      sequence
        [ copyInternalAuthComponent [relfile|social/SocialIcons.tsx|],
          copyInternalAuthComponent [relfile|social/SocialIcons.module.css|]
        ]

    isExternalAuthEnabled = AS.Auth.isExternalAuthEnabled auth
    isUsernameAndPasswordAuthEnabled = AS.Auth.isUsernameAndPasswordAuthEnabled auth
    isEmailAuthEnabled = AS.Auth.isEmailAuthEnabled auth

    copyInternalAuthComponent = return . C.mkTmplFd . (pathToInternalInAuth </>)
    pathToInternalInAuth = [reldir|auth/forms/internal|]

genLoginSignupForm :: AS.Auth.Auth -> Generator [FileDraft]
genLoginSignupForm auth =
  return
    [ C.mkTmplFdWithData
        [relfile|auth/forms/internal/common/LoginSignupForm.tsx|]
        tmplData,
      C.mkTmplFd [relfile|auth/forms/internal/common/LoginSignupForm.module.css|]
    ]
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
