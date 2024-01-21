module Wasp.Generator.SdkGenerator.Auth.LocalAuthG
  ( genLocalAuth,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (relfile)
import qualified StrongPath as SP
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.AuthProviders (localAuthProvider)
import Wasp.Generator.AuthProviders.Local (serverLoginUrl, serverSignupUrl)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common as C

genLocalAuth :: AS.Auth.Auth -> Generator [FileDraft]
genLocalAuth = genActions

genActions :: AS.Auth.Auth -> Generator [FileDraft]
genActions auth
  | AS.Auth.isUsernameAndPasswordAuthEnabled auth =
      sequence
        [ genLocalLoginAction,
          genLocalSignupAction
        ]
  | otherwise = return []

-- | Generates file with signup function to be used by Wasp developer.
genLocalSignupAction :: Generator FileDraft
genLocalSignupAction = return $ C.mkTmplFdWithData (SP.castRel [relfile|auth/signup.ts|]) tmplData
  where
    tmplData = object ["signupPath" .= serverSignupUrl localAuthProvider]

-- | Generates file with login function to be used by Wasp developer.
genLocalLoginAction :: Generator FileDraft
genLocalLoginAction = return $ C.mkTmplFdWithData (SP.castRel [relfile|auth/login.ts|]) tmplData
  where
    tmplData = object ["loginPath" .= serverLoginUrl localAuthProvider]
