{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App.Auth
  ( Auth (..),
    AuthMethod (..),
  )
where

import Data.Data (Data)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity (Entity)

data Auth = Auth
  { userEntity :: Ref Entity,
    methods :: [AuthMethod],
    onAuthFailedRedirectTo :: String,
    onAuthSucceededRedirectTo :: Maybe String
  }
  deriving (Show, Eq, Data)

data AuthMethod = UsernameAndPassword deriving (Show, Eq, Data)
