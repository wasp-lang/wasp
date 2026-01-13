module Wasp.Generator.SdkGenerator.Auth.LocalAuthG
  ( genLocalAuth,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (Dir', File', Path', Rel, Rel', reldir, relfile, (</>))
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.AuthProviders (localAuthProvider)
import Wasp.Generator.AuthProviders.Local (serverLoginUrl, serverSignupUrl)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.JsImport (extImportToImportJson)
import Wasp.Generator.SdkGenerator.UserCore.Common
  ( UserCoreTemplatesDir,
    mkTmplFd,
    mkTmplFdWithData,
  )
import Wasp.Util ((<++>))

genLocalAuth :: AS.Auth.Auth -> Generator [FileDraft]
genLocalAuth auth
  | AS.Auth.isUsernameAndPasswordAuthEnabled auth =
      sequence
        [ genLocalAuthDirFileCopy [relfile|index.ts|]
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
genLoginAction = return $ mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = localAuthDirInUserCoreTemplatesDir </> [relfile|actions/login.ts|]
    tmplData = object ["loginPath" .= serverLoginUrl localAuthProvider]

genSignupAction :: AS.Auth.Auth -> Generator FileDraft
genSignupAction auth = return $ mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = localAuthDirInUserCoreTemplatesDir </> [relfile|actions/signup.ts|]
    tmplData =
      object
        [ "signupPath" .= serverSignupUrl localAuthProvider,
          "usernameAndPasswordUserSignupFields" .= extImportToImportJson userUsernameAndPassowrdSignupFields
        ]
    userUsernameAndPassowrdSignupFields = AS.Auth.usernameAndPassword authMethods >>= AS.Auth.userSignupFieldsForUsernameAuth
    authMethods = AS.Auth.methods auth

localAuthDirInUserCoreTemplatesDir :: Path' (Rel UserCoreTemplatesDir) Dir'
localAuthDirInUserCoreTemplatesDir = [reldir|auth/username|]

genLocalAuthDirFileCopy :: Path' Rel' File' -> Generator FileDraft
genLocalAuthDirFileCopy =
  return . mkTmplFd . (localAuthDirInUserCoreTemplatesDir </>)
