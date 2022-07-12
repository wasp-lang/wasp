{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App.Auth
  ( Auth (..),
    AuthMethods (..),
    GoogleConfig (..),
    isEmailAndPasswordAuthEnabled,
    isGoogleAuthEnabled,
    isExternalAuthEnabled,
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
    onAuthSucceededRedirectTo :: Maybe String,
    onSignInFn :: ExtImport
  }
  deriving (Show, Eq, Data)

data AuthMethods = AuthMethods
  { emailAndPassword :: Maybe Bool,
    google :: Maybe GoogleConfig
  }
  deriving (Show, Eq, Data)

data GoogleConfig = GoogleConfig
  { configFn :: ExtImport
  }
  deriving (Show, Eq, Data)

isEmailAndPasswordAuthEnabled :: Auth -> Bool
isEmailAndPasswordAuthEnabled auth = Just True == (emailAndPassword . methods $ auth)

isGoogleAuthEnabled :: Auth -> Bool
isGoogleAuthEnabled = isJust . google . methods

isExternalAuthEnabled :: Auth -> Bool
isExternalAuthEnabled auth = any ($ auth) [isGoogleAuthEnabled]
