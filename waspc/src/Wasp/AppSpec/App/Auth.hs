{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Wasp.AppSpec.App.Auth
  ( Auth (..),
    AuthMethods (..),
    GoogleConfig (..),
    emailAndPasswordConfig,
    isEmailAndPasswordAuthEnabled,
    isGoogleAuthEnabled,
    isGoogleAuthEnabled',
    isExternalAuthEnabled,
    isExternalAuthEnabled',
  )
where

import Data.Data (Data)
import Data.Maybe (isJust)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity (Entity)
import Wasp.AppSpec.ExtImport (ExtImport)

data Auth = Auth
  { userEntity :: Ref Entity,
    methods :: AuthMethods,
    onAuthFailedRedirectTo :: String,
    onAuthSucceededRedirectTo :: Maybe String
  }
  deriving (Show, Eq, Data)

data AuthMethods = AuthMethods
  { emailAndPassword :: Maybe EmailAndPasswordConfig,
    google :: Maybe GoogleConfig
  }
  deriving (Show, Eq, Data)

data EmailAndPasswordConfig = EmailAndPasswordConfig
  { -- NOTE: Not used right now, but Analyzer does not support an empty data type.
    configFn :: Maybe ExtImport
  }
  deriving (Show, Eq, Data)

data GoogleConfig = GoogleConfig
  { configFn :: Maybe ExtImport,
    onSignInFn :: Maybe ExtImport
  }
  deriving (Show, Eq, Data)

emailAndPasswordConfig :: EmailAndPasswordConfig
emailAndPasswordConfig = EmailAndPasswordConfig Nothing

isEmailAndPasswordAuthEnabled :: Auth -> Bool
isEmailAndPasswordAuthEnabled = isJust . emailAndPassword . methods

isGoogleAuthEnabled :: Auth -> Bool
isGoogleAuthEnabled = isJust . google . methods

isGoogleAuthEnabled' :: Maybe Auth -> Bool
isGoogleAuthEnabled' maybeAuth = maybe False isGoogleAuthEnabled maybeAuth

isExternalAuthEnabled :: Auth -> Bool
isExternalAuthEnabled auth = any ($ auth) [isGoogleAuthEnabled]

isExternalAuthEnabled' :: Maybe Auth -> Bool
isExternalAuthEnabled' maybeAuth = maybe False isExternalAuthEnabled maybeAuth
