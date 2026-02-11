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
      genTypes,
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

genTypes :: Generator FileDraft
genTypes =
  return $ mkTmplFd [relfile|auth/forms/types.ts|]

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
    []
    <++> genLoginSignupForm auth
    <++> genEmailComponents
    <++> genUsernameAndPasswordComponents
  where
    genEmailComponents =
      genConditionally isEmailAuthEnabled $
        sequence
          [ return . mkTmplFd $ [relfile|auth/forms/internal/email/useEmail.ts|]
          ]

    genUsernameAndPasswordComponents =
      genConditionally isUsernameAndPasswordAuthEnabled $
        sequence
          [ return . mkTmplFd $ [relfile|auth/forms/internal/usernameAndPassword/useUsernameAndPassword.ts|]
          ]

    isEmailAuthEnabled = AS.Auth.isEmailAuthEnabled auth
    isUsernameAndPasswordAuthEnabled = AS.Auth.isUsernameAndPasswordAuthEnabled auth

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
