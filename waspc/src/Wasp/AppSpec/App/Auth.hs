{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Wasp.AppSpec.App.Auth
  ( Auth (..),
    AuthMethods (..),
    ExternalAuthConfig (..),
    EmailAuthConfig (..),
    UsernameAndPasswordConfig (..),
    isUsernameAndPasswordAuthEnabled,
    isExternalAuthEnabled,
    isSlackAuthEnabled,
    isDiscordAuthEnabled,
    isGoogleAuthEnabled,
    isKeycloakAuthEnabled,
    isGitHubAuthEnabled,
    isEmailAuthEnabled,
    userSignupFieldsForEmailAuth,
    userSignupFieldsForUsernameAuth,
    userSignupFieldsForExternalAuth,
  )
where

import qualified Data.Aeson as Aeson
import Data.Data (Data)
import Data.Maybe (catMaybes, isJust)
import GHC.Generics (Generic)
import Wasp.AppSpec.App.Auth.EmailVerification (EmailVerificationConfig)
import Wasp.AppSpec.App.Auth.PasswordReset (PasswordResetConfig)
import Wasp.AppSpec.App.EmailSender (EmailFromField)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity (Entity)
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.AppSpec.JSON (maybeToField)

data Auth = Auth
  { userEntity :: Ref Entity,
    externalAuthEntity :: Maybe (Ref Entity),
    methods :: AuthMethods,
    onAuthFailedRedirectTo :: String,
    onAuthSucceededRedirectTo :: Maybe String,
    onBeforeSignup :: Maybe ExtImport,
    onAfterSignup :: Maybe ExtImport,
    onAfterEmailVerified :: Maybe ExtImport,
    onBeforeOAuthRedirect :: Maybe ExtImport,
    onBeforeLogin :: Maybe ExtImport,
    onAfterLogin :: Maybe ExtImport
  }
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON)

instance Aeson.ToJSON Auth where
  toJSON auth =
    let requiredFields =
          [ "userEntity" Aeson..= userEntity auth,
            "methods" Aeson..= methods auth,
            "onAuthFailedRedirectTo" Aeson..= onAuthFailedRedirectTo auth
          ]
        optionalFields =
          [ maybeToField "externalAuthEntity" (externalAuthEntity auth),
            maybeToField "onAuthSucceededRedirectTo" (onAuthSucceededRedirectTo auth),
            maybeToField "onBeforeSignup" (onBeforeSignup auth),
            maybeToField "onAfterSignup" (onAfterSignup auth),
            maybeToField "onAfterEmailVerified" (onAfterEmailVerified auth),
            maybeToField "onBeforeOAuthRedirect" (onBeforeOAuthRedirect auth),
            maybeToField "onBeforeLogin" (onBeforeLogin auth),
            maybeToField "onAfterLogin" (onAfterLogin auth)
          ]
     in Aeson.object (requiredFields <> catMaybes optionalFields)

data AuthMethods = AuthMethods
  { usernameAndPassword :: Maybe UsernameAndPasswordConfig,
    slack :: Maybe ExternalAuthConfig,
    discord :: Maybe ExternalAuthConfig,
    google :: Maybe ExternalAuthConfig,
    gitHub :: Maybe ExternalAuthConfig,
    keycloak :: Maybe ExternalAuthConfig,
    email :: Maybe EmailAuthConfig
  }
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON)

instance Aeson.ToJSON AuthMethods where
  toJSON authMethods =
    let optionalFields =
          [ maybeToField "usernameAndPassword" (usernameAndPassword authMethods),
            maybeToField "slack" (slack authMethods),
            maybeToField "discord" (discord authMethods),
            maybeToField "google" (google authMethods),
            maybeToField "gitHub" (gitHub authMethods),
            maybeToField "keycloak" (keycloak authMethods),
            maybeToField "email" (email authMethods)
          ]
     in Aeson.object (catMaybes optionalFields)

data UsernameAndPasswordConfig = UsernameAndPasswordConfig
  { userSignupFields :: Maybe ExtImport
  }
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON)

instance Aeson.ToJSON UsernameAndPasswordConfig where
  toJSON config =
    let optionalFields =
          [ maybeToField "userSignupFields" (userSignupFields (config :: UsernameAndPasswordConfig))
          ]
     in Aeson.object (catMaybes optionalFields)

data ExternalAuthConfig = ExternalAuthConfig
  { configFn :: Maybe ExtImport,
    userSignupFields :: Maybe ExtImport
  }
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON)

instance Aeson.ToJSON ExternalAuthConfig where
  toJSON config =
    let optionalFields =
          [ maybeToField "configFn" (configFn config),
            maybeToField "userSignupFields" (userSignupFields (config :: ExternalAuthConfig))
          ]
     in Aeson.object (catMaybes optionalFields)

data EmailAuthConfig = EmailAuthConfig
  { userSignupFields :: Maybe ExtImport,
    fromField :: EmailFromField,
    emailVerification :: EmailVerificationConfig,
    passwordReset :: PasswordResetConfig
  }
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON)

instance Aeson.ToJSON EmailAuthConfig where
  toJSON config =
    let requiredFields =
          [ "fromField" Aeson..= fromField config,
            "emailVerification" Aeson..= emailVerification config,
            "passwordReset" Aeson..= passwordReset config
          ]
        optionalFields =
          [ maybeToField "userSignupFields" (userSignupFields (config :: EmailAuthConfig))
          ]
     in Aeson.object (requiredFields <> catMaybes optionalFields)

isUsernameAndPasswordAuthEnabled :: Auth -> Bool
isUsernameAndPasswordAuthEnabled = isJust . usernameAndPassword . methods

isExternalAuthEnabled :: Auth -> Bool
isExternalAuthEnabled auth =
  any
    ($ auth)
    -- NOTE: Make sure to add new external auth methods here.
    [ isSlackAuthEnabled,
      isDiscordAuthEnabled,
      isGoogleAuthEnabled,
      isGitHubAuthEnabled,
      isKeycloakAuthEnabled
    ]

isSlackAuthEnabled :: Auth -> Bool
isSlackAuthEnabled = isJust . slack . methods

isDiscordAuthEnabled :: Auth -> Bool
isDiscordAuthEnabled = isJust . discord . methods

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
