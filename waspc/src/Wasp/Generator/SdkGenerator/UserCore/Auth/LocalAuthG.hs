module Wasp.Generator.SdkGenerator.UserCore.Auth.LocalAuthG
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
import Wasp.Generator.SdkGenerator.JsImport (extImportToImportJson)
import Wasp.Generator.SdkGenerator.UserCore.Common
  ( mkTmplFd,
    mkTmplFdWithData,
  )
import Wasp.Util ((<++>))

genLocalAuth :: AS.Auth.Auth -> Generator [FileDraft]
genLocalAuth auth
  | AS.Auth.isUsernameAndPasswordAuthEnabled auth =
      sequence
        [ return . mkTmplFd $ [relfile|auth/username/index.ts|]
        ]
        <++> genActions auth
  | otherwise = return []

genActions :: AS.Auth.Auth -> Generator [FileDraft]
genActions auth =
  sequence
    [ genLoginAction,
      genSignupAction auth
    ]

genLoginAction :: Generator FileDraft
genLoginAction =
  return $ mkTmplFdWithData [relfile|auth/username/actions/login.ts|] tmplData
  where
    tmplData = object ["loginPath" .= serverLoginUrl localAuthProvider]

genSignupAction :: AS.Auth.Auth -> Generator FileDraft
genSignupAction auth =
  return $ mkTmplFdWithData [relfile|auth/username/actions/signup.ts|] tmplData
  where
    tmplData =
      object
        [ "signupPath" .= serverSignupUrl localAuthProvider,
          "usernameAndPasswordUserSignupFields" .= extImportToImportJson userUsernameAndPassowrdSignupFields
        ]
    userUsernameAndPassowrdSignupFields = AS.Auth.usernameAndPassword authMethods >>= AS.Auth.userSignupFieldsForUsernameAuth
    authMethods = AS.Auth.methods auth
