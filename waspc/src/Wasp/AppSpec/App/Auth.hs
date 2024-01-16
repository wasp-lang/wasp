{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Wasp.AppSpec.App.Auth
  ( Auth (..),
    AuthMethods (..),
    ExternalAuthConfig (..),
    EmailAuthConfig (..),
    UsernameAndPasswordConfig (..),
    isUsernameAndPasswordAuthEnabled,
    isExternalAuthEnabled,
    isGoogleAuthEnabled,
    isGitHubAuthEnabled,
    isEmailAuthEnabled,
    getEmailUserFieldsFn,
    getUsernameAndPasswordUserFieldsFn,
    getExternalAuthUserFieldsFn,
  )
where

import Data.Data (Data)
import Data.Maybe (isJust)
import Wasp.AppSpec.App.Auth.EmailVerification (EmailVerificationConfig)
import Wasp.AppSpec.App.Auth.PasswordReset (PasswordResetConfig)
import Wasp.AppSpec.App.EmailSender (EmailFromField)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity (Entity)
import Wasp.AppSpec.ExtImport (ExtImport)

data Auth = Auth
  { userEntity :: Ref Entity,
    externalAuthEntity :: Maybe (Ref Entity),
    methods :: AuthMethods,
    onAuthFailedRedirectTo :: String,
    onAuthSucceededRedirectTo :: Maybe String
  }
  deriving (Show, Eq, Data)

data AuthMethods = AuthMethods
  { usernameAndPassword :: Maybe UsernameAndPasswordConfig,
    google :: Maybe ExternalAuthConfig,
    gitHub :: Maybe ExternalAuthConfig,
    email :: Maybe EmailAuthConfig
  }
  deriving (Show, Eq, Data)

data UsernameAndPasswordConfig = UsernameAndPasswordConfig
  { getUserFieldsFn :: Maybe ExtImport
  }
  deriving (Show, Eq, Data)

data ExternalAuthConfig = ExternalAuthConfig
  { configFn :: Maybe ExtImport,
    getUserFieldsFn :: Maybe ExtImport
  }
  deriving (Show, Eq, Data)

data EmailAuthConfig = EmailAuthConfig
  { getUserFieldsFn :: Maybe ExtImport,
    fromField :: EmailFromField,
    emailVerification :: EmailVerificationConfig,
    passwordReset :: PasswordResetConfig
  }
  deriving (Show, Eq, Data)

isUsernameAndPasswordAuthEnabled :: Auth -> Bool
isUsernameAndPasswordAuthEnabled = isJust . usernameAndPassword . methods

isExternalAuthEnabled :: Auth -> Bool
isExternalAuthEnabled auth = any ($ auth) [isGoogleAuthEnabled, isGitHubAuthEnabled]

isGoogleAuthEnabled :: Auth -> Bool
isGoogleAuthEnabled = isJust . google . methods

isGitHubAuthEnabled :: Auth -> Bool
isGitHubAuthEnabled = isJust . gitHub . methods

isEmailAuthEnabled :: Auth -> Bool
isEmailAuthEnabled = isJust . email . methods

-- These helper functions are used to avoid ambiguity when using the
-- `getUserFieldsFn` function (otherwise we need to use the DuplicateRecordFields
-- extension in each module that uses them).
getEmailUserFieldsFn :: EmailAuthConfig -> Maybe ExtImport
getEmailUserFieldsFn = getUserFieldsFn

getUsernameAndPasswordUserFieldsFn :: UsernameAndPasswordConfig -> Maybe ExtImport
getUsernameAndPasswordUserFieldsFn = getUserFieldsFn

getExternalAuthUserFieldsFn :: ExternalAuthConfig -> Maybe ExtImport
getExternalAuthUserFieldsFn = getUserFieldsFn
