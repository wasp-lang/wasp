{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App.Auth
  ( Auth (..),
    AuthMethods (..),
    GoogleConfig (..),
    emailAndPasswordAuthEnabled,
    googleAuthEnabled,
    passportRequired,
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
    onSignInFn :: Maybe ExtImport
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

emailAndPasswordAuthEnabled :: Auth -> Bool
emailAndPasswordAuthEnabled auth = Just True == (emailAndPassword . methods $ auth)

googleAuthEnabled :: Auth -> Bool
googleAuthEnabled = isJust . google . methods

passportRequired :: Auth -> Bool
passportRequired auth = any ($ auth) [googleAuthEnabled]
