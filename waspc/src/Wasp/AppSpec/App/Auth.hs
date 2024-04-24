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
    isKeycloakAuthEnabled,
    isGitHubAuthEnabled,
    isEmailAuthEnabled,
    userSignupFieldsForEmailAuth,
    userSignupFieldsForUsernameAuth,
    userSignupFieldsForExternalAuth,
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
    onAuthSucceededRedirectTo :: Maybe String,
    onBeforeSignup :: Maybe ExtImport,
    onAfterSignup :: Maybe ExtImport,
    onBeforeOAuthRedirect :: Maybe ExtImport,
    onAfterOAuthTokenReceived :: Maybe ExtImport
  }
  deriving (Show, Eq, Data)

data AuthMethods = AuthMethods
  { usernameAndPassword :: Maybe UsernameAndPasswordConfig,
    google :: Maybe ExternalAuthConfig,
    gitHub :: Maybe ExternalAuthConfig,
    keycloak :: Maybe ExternalAuthConfig,
    email :: Maybe EmailAuthConfig
  }
  deriving (Show, Eq, Data)

data UsernameAndPasswordConfig = UsernameAndPasswordConfig
  { userSignupFields :: Maybe ExtImport
  }
  deriving (Show, Eq, Data)

data ExternalAuthConfig = ExternalAuthConfig
  { configFn :: Maybe ExtImport,
    userSignupFields :: Maybe ExtImport
  }
  deriving (Show, Eq, Data)

data EmailAuthConfig = EmailAuthConfig
  { userSignupFields :: Maybe ExtImport,
    fromField :: EmailFromField,
    emailVerification :: EmailVerificationConfig,
    passwordReset :: PasswordResetConfig
  }
  deriving (Show, Eq, Data)

isUsernameAndPasswordAuthEnabled :: Auth -> Bool
isUsernameAndPasswordAuthEnabled = isJust . usernameAndPassword . methods

isExternalAuthEnabled :: Auth -> Bool
isExternalAuthEnabled auth =
  any
    ($ auth)
    -- NOTE: Make sure to add new external auth methods here.
    [ isGoogleAuthEnabled,
      isGitHubAuthEnabled,
      isKeycloakAuthEnabled
    ]

isGoogleAuthEnabled :: Auth -> Bool
isGoogleAuthEnabled = isJust . google . methods

isKeycloakAuthEnabled :: Auth -> Bool
isKeycloakAuthEnabled = isJust . keycloak . methods

isGitHubAuthEnabled :: Auth -> Bool
isGitHubAuthEnabled = isJust . gitHub . methods

isEmailAuthEnabled :: Auth -> Bool
isEmailAuthEnabled = isJust . email . methods

-- These helper functions are used to avoid ambiguity when using the
-- `userSignupFields` function (otherwise we need to use the DuplicateRecordFields
-- extension in each module that uses them).
userSignupFieldsForEmailAuth :: EmailAuthConfig -> Maybe ExtImport
userSignupFieldsForEmailAuth = userSignupFields

userSignupFieldsForUsernameAuth :: UsernameAndPasswordConfig -> Maybe ExtImport
userSignupFieldsForUsernameAuth = userSignupFields

userSignupFieldsForExternalAuth :: ExternalAuthConfig -> Maybe ExtImport
userSignupFieldsForExternalAuth = userSignupFields
