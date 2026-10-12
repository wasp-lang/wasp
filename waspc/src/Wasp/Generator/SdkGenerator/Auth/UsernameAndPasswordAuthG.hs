module Wasp.Generator.SdkGenerator.Auth.UsernameAndPasswordAuthG
  ( genUsernameAndPasswordAuth,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (isJust)
import StrongPath (Dir', File', Path', Rel, Rel', reldir, relfile, (</>))
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.AuthProviders (usernameAndPasswordAuthProvider)
import Wasp.Generator.AuthProviders.UsernameAndPassword (serverLoginUrl, serverSignupUrl)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common
  ( SdkTemplatesDir,
    genFileCopy,
    mkTmplFdWithData,
  )
import Wasp.Util ((<++>))

genUsernameAndPasswordAuth :: AS.Auth.Auth -> Generator [FileDraft]
genUsernameAndPasswordAuth auth
  | AS.Auth.isUsernameAndPasswordAuthEnabled auth =
      sequence
        [ genFileCopyInUsernameAndPasswordAuthDir [relfile|index.ts|]
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
  return $
    mkTmplFdWithData
      (usernameAndPasswordAuthDirInSdkTemplatesDir </> [relfile|actions/login.ts|])
      tmplData
  where
    tmplData = object ["loginPath" .= serverLoginUrl usernameAndPasswordAuthProvider]

genSignupAction :: AS.Auth.Auth -> Generator FileDraft
genSignupAction auth =
  return $
    mkTmplFdWithData
      (usernameAndPasswordAuthDirInSdkTemplatesDir </> [relfile|actions/signup.ts|])
      tmplData
  where
    tmplData =
      object
        [ "signupPath" .= serverSignupUrl usernameAndPasswordAuthProvider,
          "isUsernameAndPasswordUserSignupFieldsDefined" .= isJust usernameAndPasswordUserSignupFields
        ]
    usernameAndPasswordUserSignupFields = AS.Auth.usernameAndPassword authMethods >>= AS.Auth.userSignupFieldsForUsernameAuth
    authMethods = AS.Auth.methods auth

usernameAndPasswordAuthDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) Dir'
usernameAndPasswordAuthDirInSdkTemplatesDir = [reldir|auth/username|]

genFileCopyInUsernameAndPasswordAuthDir :: Path' Rel' File' -> Generator FileDraft
genFileCopyInUsernameAndPasswordAuthDir =
  genFileCopy . (usernameAndPasswordAuthDirInSdkTemplatesDir </>)
