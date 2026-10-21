module Wasp.Generator.SdkGenerator.Auth.UsernameAndPasswordAuthG
  ( genUsernameAndPasswordAuth,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (isJust)
import StrongPath (relfile)
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.AuthProviders (usernameAndPasswordAuthProvider)
import Wasp.Generator.AuthProviders.UsernameAndPassword (serverLoginUrl, serverSignupUrl)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common (mkTmplFdWithData)

genUsernameAndPasswordAuth :: AS.Auth.Auth -> Generator [FileDraft]
genUsernameAndPasswordAuth auth
  | AS.Auth.isUsernameAndPasswordAuthEnabled auth = return [genActions auth]
  | otherwise = return []

-- | Generates the file that creates the client username and password auth
--   actions from `@wasp.sh/lib-auth` with the app's server paths.
genActions :: AS.Auth.Auth -> FileDraft
genActions auth = mkTmplFdWithData [relfile|auth/username/actions.ts|] tmplData
  where
    tmplData =
      object
        [ "loginPath" .= serverLoginUrl usernameAndPasswordAuthProvider,
          "signupPath" .= serverSignupUrl usernameAndPasswordAuthProvider,
          "isUsernameAndPasswordUserSignupFieldsDefined" .= isJust usernameAndPasswordUserSignupFields
        ]
    usernameAndPasswordUserSignupFields = AS.Auth.usernameAndPassword authMethods >>= AS.Auth.userSignupFieldsForUsernameAuth
    authMethods = AS.Auth.methods auth
