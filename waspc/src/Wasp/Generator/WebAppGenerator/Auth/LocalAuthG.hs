module Wasp.Generator.WebAppGenerator.Auth.LocalAuthG
  ( genLocalAuth,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (relfile)
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.AuthProviders (localAuthProvider)
import Wasp.Generator.AuthProviders.Local (serverLoginUrl, serverSignupUrl)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.WebAppGenerator.Common as C

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
genLocalSignupAction = return $ C.mkTmplFdWithData (C.asTmplFile [relfile|src/auth/signup.js|]) tmplData
  where
    tmplData = object ["signupPath" .= serverSignupUrl localAuthProvider]

-- | Generates file with login function to be used by Wasp developer.
genLocalLoginAction :: Generator FileDraft
genLocalLoginAction = return $ C.mkTmplFdWithData (C.asTmplFile [relfile|src/auth/login.js|]) tmplData
  where
    tmplData = object ["loginPath" .= serverLoginUrl localAuthProvider]
