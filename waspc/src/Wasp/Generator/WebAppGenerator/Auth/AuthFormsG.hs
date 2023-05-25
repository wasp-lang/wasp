module Wasp.Generator.WebAppGenerator.Auth.AuthFormsG
  ( genAuthForms,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (reldir, relfile, (</>))
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.AuthProviders (gitHubAuthProvider, googleAuthProvider)
import qualified Wasp.Generator.AuthProviders.OAuth as OAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.WebAppGenerator.Auth.Common (getOnAuthSucceededRedirectToOrDefault)
import Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Util ((<++>))

genAuthForms :: AS.Auth.Auth -> Generator [FileDraft]
genAuthForms auth =
  sequence
    [ genAuthComponent auth,
      genTypes auth,
      copyTmplFile [relfile|auth/forms/Login.tsx|],
      copyTmplFile [relfile|auth/forms/Signup.tsx|],
      copyTmplFile [relfile|stitches.config.js|]
    ]
    <++> genEmailForms auth
    <++> genInternalAuthComponents auth
  where
    copyTmplFile = return . C.mkSrcTmplFd

genAuthComponent :: AS.Auth.Auth -> Generator FileDraft
genAuthComponent auth =
  return $
    C.mkTmplFdWithData
      [relfile|src/auth/forms/Auth.tsx|]
      tmplData
  where
    tmplData = object ["isEmailAuthEnabled" .= AS.Auth.isEmailAuthEnabled auth]

genTypes :: AS.Auth.Auth -> Generator FileDraft
genTypes auth =
  return $
    C.mkTmplFdWithData
      [relfile|src/auth/forms/types.ts|]
      tmplData
  where
    tmplData = object ["isEmailAuthEnabled" .= AS.Auth.isEmailAuthEnabled auth]

genEmailForms :: AS.Auth.Auth -> Generator [FileDraft]
genEmailForms auth =
  genConditionally isEmailAuthEnabled $
    sequence
      [ copyTmplFile [relfile|auth/forms/ResetPassword.tsx|],
        copyTmplFile [relfile|auth/forms/ForgotPassword.tsx|],
        copyTmplFile [relfile|auth/forms/VerifyEmail.tsx|]
      ]
  where
    copyTmplFile = return . C.mkSrcTmplFd
    isEmailAuthEnabled = AS.Auth.isEmailAuthEnabled auth

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

    isExternalAuthEnabled = AS.Auth.isExternalAuthEnabled auth
    isUsernameAndPasswordAuthEnabled = AS.Auth.isUsernameAndPasswordAuthEnabled auth
    isEmailAuthEnabled = AS.Auth.isEmailAuthEnabled auth

    copyInternalAuthComponent = return . C.mkSrcTmplFd . (pathToInternalInAuth </>)
    pathToInternalInAuth = [reldir|auth/forms/internal|]

genLoginSignupForm :: AS.Auth.Auth -> Generator FileDraft
genLoginSignupForm auth =
  return $
    C.mkTmplFdWithData
      [relfile|src/auth/forms/internal/common/LoginSignupForm.tsx|]
      tmplData
  where
    tmplData =
      object
        [ "onAuthSucceededRedirectTo" .= getOnAuthSucceededRedirectToOrDefault auth,
          "areBothSocialAndPasswordBasedAuthEnabled" .= areBothSocialAndPasswordBasedAuthEnabled,
          "isAnyPasswordBasedAuthEnabled" .= isAnyPasswordBasedAuthEnabled,
          "isExternalAuthEnabled" .= AS.Auth.isExternalAuthEnabled auth,
          -- Google
          "isGoogleAuthEnabled" .= AS.Auth.isGoogleAuthEnabled auth,
          "googleSignInPath" .= OAuth.serverLoginUrl googleAuthProvider,
          -- GitHub
          "isGitHubAuthEnabled" .= AS.Auth.isGitHubAuthEnabled auth,
          "gitHubSignInPath" .= OAuth.serverLoginUrl gitHubAuthProvider,
          -- Username and password
          "isUsernameAndPasswordAuthEnabled" .= AS.Auth.isUsernameAndPasswordAuthEnabled auth,
          -- Email
          "isEmailAuthEnabled" .= AS.Auth.isEmailAuthEnabled auth,
          "isEmailVerificationRequired" .= AS.Auth.isEmailVerificationRequired auth
        ]
    areBothSocialAndPasswordBasedAuthEnabled = AS.Auth.isExternalAuthEnabled auth && isAnyPasswordBasedAuthEnabled
    isAnyPasswordBasedAuthEnabled = AS.Auth.isUsernameAndPasswordAuthEnabled auth || AS.Auth.isEmailAuthEnabled auth

genConditionally :: Bool -> Generator [FileDraft] -> Generator [FileDraft]
genConditionally isEnabled gen = if isEnabled then gen else return []
