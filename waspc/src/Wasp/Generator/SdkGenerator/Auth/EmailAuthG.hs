module Wasp.Generator.SdkGenerator.Auth.EmailAuthG
  ( genEmailAuth,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (Dir', Path', Rel, castRel, reldir, relfile, (</>))
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
import Wasp.Generator.SdkGenerator.Common
import Wasp.Generator.SdkGenerator.JsImport (extImportToImportJson)
import Wasp.Generator.SdkGenerator.Server.Common
import Wasp.Util ((<++>))
import qualified Wasp.Util as Util

emailAuthDirInSdkTemplatesProjectDir :: Path' (Rel SdkTemplatesProjectDir) Dir'
emailAuthDirInSdkTemplatesProjectDir = [reldir|auth/email|]

genEmailAuth :: AS.Auth.Auth -> Generator [FileDraft]
genEmailAuth auth
  | AS.Auth.isEmailAuthEnabled auth =
      sequence
        [ genIndex,
          genServerUtils auth
        ]
        <++> genActions auth
  | otherwise = return []

genIndex :: Generator FileDraft
genIndex =
  return $
    makeSdkProjectTmplFd SdkUserCoreProject tmplFile
  where
    tmplFile = emailAuthDirInSdkTemplatesProjectDir </> [relfile|index.ts|]

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
    makeSdkProjectTmplFdWithData SdkUserCoreProject tmplFile tmplData
  where
    tmplFile = emailAuthDirInSdkTemplatesProjectDir </> [relfile|actions/login.ts|]
    tmplData = object ["loginPath" .= serverLoginUrl emailAuthProvider]

genSignupAction :: AS.Auth.Auth -> Generator FileDraft
genSignupAction auth =
  return $
    makeSdkProjectTmplFdWithData SdkUserCoreProject tmplFile tmplData
  where
    tmplFile = emailAuthDirInSdkTemplatesProjectDir </> [relfile|actions/signup.ts|]
    tmplData =
      object
        [ "signupPath" .= serverSignupUrl emailAuthProvider,
          "emailUserSignupFields" .= extImportToImportJson userEmailSignupFields
        ]
    userEmailSignupFields = AS.Auth.email authMethods >>= AS.Auth.userSignupFieldsForEmailAuth
    authMethods = AS.Auth.methods auth

genPasswordResetActions :: Generator FileDraft
genPasswordResetActions =
  return $
    makeSdkProjectTmplFdWithData SdkUserCoreProject tmplFile tmplData
  where
    tmplFile = emailAuthDirInSdkTemplatesProjectDir </> [relfile|actions/passwordReset.ts|]
    tmplData =
      object
        [ "requestPasswordResetPath" .= serverRequestPasswordResetUrl emailAuthProvider,
          "resetPasswordPath" .= serverResetPasswordUrl emailAuthProvider
        ]

genVerifyEmailAction :: Generator FileDraft
genVerifyEmailAction =
  return $
    makeSdkProjectTmplFdWithData SdkUserCoreProject tmplFile tmplData
  where
    tmplFile = emailAuthDirInSdkTemplatesProjectDir </> [relfile|actions/verifyEmail.ts|]
    tmplData = object ["verifyEmailPath" .= serverVerifyEmailUrl emailAuthProvider]

genServerUtils :: AS.Auth.Auth -> Generator FileDraft
genServerUtils auth =
  return $
    makeSdkProjectTmplFdWithData SdkUserCoreProject tmplFile tmplData
  where
    -- TODO(franjo): This one is server specific so we have to 'castRel' since the rest
    -- of code assumes the email auth path is relative to 'SdkTemplatesProjectDir'.
    -- Do we move this out to 'Server.Auth.EmailAuthG.hs'?
    tmplFile = serverTemplatesDirInSdkTemplatesDir </> castRel emailAuthDirInSdkTemplatesProjectDir </> [relfile|utils.ts|]
    tmplData =
      object
        [ "userEntityUpper" .= (userEntityName :: String),
          "userEntityLower" .= (Util.toLowerFirst userEntityName :: String),
          "authEntityUpper" .= (DbAuth.authEntityName :: String),
          "authEntityLower" .= (Util.toLowerFirst DbAuth.authEntityName :: String),
          "userFieldOnAuthEntityName" .= (DbAuth.userFieldOnAuthEntityName :: String)
        ]
    userEntityName = AS.refName $ AS.Auth.userEntity auth
