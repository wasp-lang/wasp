module Wasp.Generator.SdkGenerator.Auth.EmailAuthG
  ( genEmailAuth,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (isJust)
import StrongPath (relfile)
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
import Wasp.Generator.SdkGenerator.Common (mkTmplFdWithData)

genEmailAuth :: AS.Auth.Auth -> Generator [FileDraft]
genEmailAuth auth
  | AS.Auth.isEmailAuthEnabled auth = return [genActions auth]
  | otherwise = return []

-- | Generates the file that creates the client email auth actions from
--   `@wasp.sh/lib-auth` with the app's server paths.
genActions :: AS.Auth.Auth -> FileDraft
genActions auth = mkTmplFdWithData [relfile|auth/email/actions.ts|] tmplData
  where
    tmplData =
      object
        [ "loginPath" .= serverLoginUrl emailAuthProvider,
          "signupPath" .= serverSignupUrl emailAuthProvider,
          "requestPasswordResetPath" .= serverRequestPasswordResetUrl emailAuthProvider,
          "resetPasswordPath" .= serverResetPasswordUrl emailAuthProvider,
          "verifyEmailPath" .= serverVerifyEmailUrl emailAuthProvider,
          "isEmailUserSignupFieldsDefined" .= isJust emailUserSignupFields
        ]
    emailUserSignupFields = AS.Auth.email authMethods >>= AS.Auth.userSignupFieldsForEmailAuth
    authMethods = AS.Auth.methods auth
