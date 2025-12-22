{-# LANGUAGE DeriveGeneric #-}

module Wasp.Project.BuildType where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data BuildType
  = -- | Project is being compiled for development purposes (e.g. "wasp start").
    Development
  | -- | Project is being compiled for production/deployment (e.g. "wasp build").
    Production
  deriving (Eq, Show, Generic)

instance FromJSON BuildType

instance ToJSON BuildType
