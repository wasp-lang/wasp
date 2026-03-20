module Wasp.Generator.SdkGenerator.Auth.EmailAuthG
  ( genEmailAuth,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (Dir', File', Path', Rel, Rel', reldir, relfile, (</>))
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.Auth.Provider (isEmailEnabled)
import qualified Wasp.Generator.DbGenerator.Auth as DbAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common
  ( SdkTemplatesDir,
    genFileCopy,
    mkTmplFdWithData,
  )
import Wasp.Generator.SdkGenerator.JsImport (extImportToImportJson)
import Wasp.Util ((<++>))
import qualified Wasp.Util as Util

genEmailAuth :: AS.Auth.Auth -> Generator [FileDraft]
genEmailAuth auth
  | isEmailEnabled auth =
      sequence
        [ genFileCopyInEmailAuthDir [relfile|index.ts|],
          genServerUtils auth
        ]
        <++> genActions auth
  | otherwise = return []

genServerUtils :: AS.Auth.Auth -> Generator FileDraft
genServerUtils auth =
  return $
    mkTmplFdWithData
      [relfile|server/auth/email/utils.ts|]
      tmplData
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
    [ genLoginAction,
      genSignupAction auth,
      genPasswordResetActions,
      genVerifyEmailAction
    ]

genLoginAction :: Generator FileDraft
genLoginAction =
  return $
    mkTmplFdWithData
      (emailAuthDirInSdkTemplatesDir </> [relfile|actions/login.ts|])
      tmplData
  where
    tmplData = object ["loginPath" .= ("/auth/email/login" :: String)]

genSignupAction :: AS.Auth.Auth -> Generator FileDraft
genSignupAction auth =
  return $
    mkTmplFdWithData
      (emailAuthDirInSdkTemplatesDir </> [relfile|actions/signup.ts|])
      tmplData
  where
    tmplData =
      object
        [ "signupPath" .= ("/auth/email/signup" :: String),
          "emailUserSignupFields" .= extImportToImportJson userEmailSignupFields
        ]
    userEmailSignupFields = AS.Auth.email authMethods >>= AS.Auth.userSignupFieldsForEmailAuth
    authMethods = AS.Auth.methods auth

genPasswordResetActions :: Generator FileDraft
genPasswordResetActions =
  return $
    mkTmplFdWithData
      (emailAuthDirInSdkTemplatesDir </> [relfile|actions/passwordReset.ts|])
      tmplData
  where
    tmplData =
      object
        [ "requestPasswordResetPath" .= ("/auth/email/request-password-reset" :: String),
          "resetPasswordPath" .= ("/auth/email/reset-password" :: String)
        ]

genVerifyEmailAction :: Generator FileDraft
genVerifyEmailAction =
  return $
    mkTmplFdWithData
      (emailAuthDirInSdkTemplatesDir </> [relfile|actions/verifyEmail.ts|])
      tmplData
  where
    tmplData = object ["verifyEmailPath" .= ("/auth/email/verify-email" :: String)]

emailAuthDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) Dir'
emailAuthDirInSdkTemplatesDir = [reldir|auth/email|]

genFileCopyInEmailAuthDir :: Path' Rel' File' -> Generator FileDraft
genFileCopyInEmailAuthDir =
  genFileCopy . (emailAuthDirInSdkTemplatesDir </>)
