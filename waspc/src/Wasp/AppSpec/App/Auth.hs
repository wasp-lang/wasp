{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Wasp.AppSpec.App.Auth
  ( Auth (..),
    AuthMethods (..),
    ExternalAuthConfig (..),
    usernameAndPasswordConfig,
    isUsernameAndPasswordAuthEnabled,
    isBothExternalAndUsernameAndPasswordAuthEnabled,
    isExternalAuthEnabled,
    isGoogleAuthEnabled,
    isGitHubAuthEnabled,
  )
where

import Data.Data (Data)
import Data.Maybe (isJust)
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
    gitHub :: Maybe ExternalAuthConfig
  }
  deriving (Show, Eq, Data)

data UsernameAndPasswordConfig = UsernameAndPasswordConfig
  { -- NOTE: Not used right now, but Analyzer does not support an empty data type.
    configFn :: Maybe ExtImport
  }
  deriving (Show, Eq, Data)

data ExternalAuthConfig = ExternalAuthConfig
  { configFn :: Maybe ExtImport,
    getUserFieldsFn :: Maybe ExtImport
  }
  deriving (Show, Eq, Data)

usernameAndPasswordConfig :: UsernameAndPasswordConfig
usernameAndPasswordConfig = UsernameAndPasswordConfig Nothing

isUsernameAndPasswordAuthEnabled :: Auth -> Bool
isUsernameAndPasswordAuthEnabled = isJust . usernameAndPassword . methods

isBothExternalAndUsernameAndPasswordAuthEnabled :: Auth -> Bool
isBothExternalAndUsernameAndPasswordAuthEnabled auth = all ($ auth) [isExternalAuthEnabled, isUsernameAndPasswordAuthEnabled]

isExternalAuthEnabled :: Auth -> Bool
isExternalAuthEnabled auth = any ($ auth) [isGoogleAuthEnabled, isGitHubAuthEnabled]

isGoogleAuthEnabled :: Auth -> Bool
isGoogleAuthEnabled = isJust . google . methods

isGitHubAuthEnabled :: Auth -> Bool
isGitHubAuthEnabled = isJust . gitHub . methods
