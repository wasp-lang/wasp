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
      copyTmplFile [relfile|auth/forms/Login.tsx|],
      copyTmplFile [relfile|auth/forms/Signup.tsx|],
      copyTmplFile [relfile|auth/forms/ResetPassword.tsx|],
      copyTmplFile [relfile|auth/forms/ForgotPassword.tsx|],
      copyTmplFile [relfile|auth/forms/VerifyEmail.tsx|],
      copyTmplFile [relfile|auth/forms/types.ts|],
      copyTmplFile [relfile|stitches.config.js|]
    ]
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

genInternalAuthComponents :: AS.Auth.Auth -> Generator [FileDraft]
genInternalAuthComponents auth =
  sequence
    [ copyInternalAuthComponent [relfile|usernameAndPassword/useUsernameAndPassword.ts|],
      copyInternalAuthComponent [relfile|email/VerifyEmailForm.tsx|],
      copyInternalAuthComponent [relfile|email/useEmail.ts|],
      copyInternalAuthComponent [relfile|email/ForgotPasswordForm.tsx|],
      copyInternalAuthComponent [relfile|email/ResetPasswordForm.tsx|],
      copyInternalAuthComponent [relfile|social/SocialButton.tsx|],
      copyInternalAuthComponent [relfile|social/SocialIcons.tsx|],
      copyInternalAuthComponent [relfile|Form.tsx|],
      copyInternalAuthComponent [relfile|Message.tsx|],
      genLoginSignupForm auth
    ]
  where
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
