{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App.Auth
  ( Auth (..),
    AuthMethod (..),
    getSessionEntityName,
  )
where

import Data.Data (Data)
import Data.Maybe (fromMaybe)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity (Entity)

data Auth = Auth
  { userEntity :: Ref Entity,
    methods :: [AuthMethod],
    onAuthFailedRedirectTo :: String,
    onAuthSucceededRedirectTo :: Maybe String,
    sessionEntityName :: Maybe String
  }
  deriving (Show, Eq, Data)

data AuthMethod = EmailAndPassword deriving (Show, Eq, Data)

defaultSessionEntityName :: String
defaultSessionEntityName = "Session"

getSessionEntityName :: Maybe Auth -> String
getSessionEntityName = fromMaybe defaultSessionEntityName . (sessionEntityName =<<)
