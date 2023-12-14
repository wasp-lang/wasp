{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.PackageJson where

import Data.Aeson (FromJSON)
import Data.Map (Map)
import GHC.Generics (Generic)

data PackageJson = PackageJson
  { name :: String,
    -- todo(filip): do this properly once you merge martin's PR
    dependencies :: Map String String,
    devDependencies :: Map String String
  }
  deriving (Show, Generic)

instance FromJSON PackageJson