module Wasp.Generator.SdkGenerator.UserCore.Auth.EmailAuthG
  ( genEmailAuth,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (relfile)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.AuthProviders (emailAuthProvider)
import Wasp.Generator.AuthProviders.Email
  ( serverLoginUrl,
    serverRequestPasswordResetUrl,
    serverResetPasswordUrl,
    serverSignupUrl,
    serverVerifyEmailUrl,
  )
import qualified Wasp.Generator.DbGenerator.Auth as DbAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.JsImport (extImportToImportJson)
import Wasp.Generator.SdkGenerator.UserCore.Common
  ( mkTmplFd,
    mkTmplFdWithData,
  )
import Wasp.Util ((<++>))
import qualified Wasp.Util as Util

genEmailAuth :: AS.Auth.Auth -> Generator [FileDraft]
genEmailAuth auth
  | AS.Auth.isEmailAuthEnabled auth =
      sequence
        [ return . mkTmplFd $ [relfile|auth/email/index.ts|],
          genServerUtils auth
        ]
        <++> genActions auth
  | otherwise = return []

genServerUtils :: AS.Auth.Auth -> Generator FileDraft
genServerUtils auth =
  return $ mkTmplFdWithData [relfile|server/auth/email/utils.ts|] tmplData
  where
    tmplData =
      object
        [ "userEntityUpper" .= (userEntityName :: String),
          "userEntityLower" .= (Util.toLowerFirst userEntityName :: String),
          "authEntityUpper" .= (DbAuth.authEntityName :: String),
          "authEntityLower" .= (Util.toLowerFirst DbAuth.authEntityName :: String),
          "userFieldOnAuthEntityName" .= (DbAuth.userFieldOnAuthEntityName :: String)
        ]
    userEntityName = AS.refName $ AS.Auth.userEntity auth

genActions :: AS.Auth.Auth -> Generator [FileDraft]
genActions auth =
  sequence
    [ genSignupAction auth
    ]

genSignupAction :: AS.Auth.Auth -> Generator FileDraft
genSignupAction auth =
  return $ mkTmplFdWithData [relfile|auth/email/actions/signup.ts|] tmplData
  where
    tmplData =
      object
        [ "signupPath" .= serverSignupUrl emailAuthProvider,
          "emailUserSignupFields" .= extImportToImportJson userEmailSignupFields
        ]
    userEmailSignupFields = AS.Auth.email authMethods >>= AS.Auth.userSignupFieldsForEmailAuth
    authMethods = AS.Auth.methods auth
