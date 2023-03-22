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
import Wasp.Generator.WebAppGenerator.Auth.Common (getOnAuthSucceededRedirectToOrDefault)
import Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Util ((<++>))

genLocalAuth :: AS.Auth.Auth -> Generator [FileDraft]
genLocalAuth auth =
  genActions auth
    <++> genForms auth

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

genForms :: AS.Auth.Auth -> Generator [FileDraft]
genForms auth
  | AS.Auth.isUsernameAndPasswordAuthEnabled auth =
      sequence
        [ genLoginForm auth,
          genSignupForm auth
        ]
  | otherwise = return []

genLoginForm :: AS.Auth.Auth -> Generator FileDraft
genLoginForm auth =
  return $
    C.mkTmplFdWithData
      [relfile|src/auth/forms/Login.jsx|]
      (object ["onAuthSucceededRedirectTo" .= getOnAuthSucceededRedirectToOrDefault auth])

genSignupForm :: AS.Auth.Auth -> Generator FileDraft
genSignupForm auth =
  return $
    C.mkTmplFdWithData
      [relfile|src/auth/forms/Signup.jsx|]
      (object ["onAuthSucceededRedirectTo" .= getOnAuthSucceededRedirectToOrDefault auth])
