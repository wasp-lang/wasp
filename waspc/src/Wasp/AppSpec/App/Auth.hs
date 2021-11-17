{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App.Auth (Auth (..)) where

import Data.Data (Data)
import Wasp.AppSpec.AuthMethod (AuthMethod)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity (Entity)

data Auth = Auth
  { userEntity :: Ref Entity,
    methods :: [AuthMethod],
    onAuthFailedRedirectTo :: Maybe String
  }
  deriving (Show, Eq, Data)
