{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.PackageJson where

import Data.Aeson (FromJSON)
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Wasp.AppSpec.App.Dependency (Dependency)
import qualified Wasp.AppSpec.App.Dependency as D

data PackageJson = PackageJson
  { name :: !String,
    dependencies :: !(Map String String),
    devDependencies :: !(Map String String)
  }
  deriving (Show, Generic)

instance FromJSON PackageJson

getDependencies :: PackageJson -> [Dependency]
getDependencies packageJson = D.fromList $ M.toList $ dependencies packageJson

getDevDependencies :: PackageJson -> [Dependency]
getDevDependencies packageJson = D.fromList $ M.toList $ devDependencies packageJson
