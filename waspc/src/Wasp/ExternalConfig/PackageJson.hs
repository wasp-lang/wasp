{-# LANGUAGE DeriveGeneric #-}

module Wasp.ExternalConfig.PackageJson
  ( PackageJson (..),
    DependenciesMap,
    PackageName,
    PackageVersion,
    getDependencies,
    getDevDependencies,
  )
where

import Data.Aeson (FromJSON)
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Wasp.AppSpec.App.Dependency (Dependency)
import qualified Wasp.AppSpec.App.Dependency as D

data PackageJson = PackageJson
  { name :: !String,
    dependencies :: !DependenciesMap,
    devDependencies :: !DependenciesMap
  }
  deriving (Show, Generic)

getDependencies :: PackageJson -> [Dependency]
getDependencies packageJson = D.fromList $ M.toList $ dependencies packageJson

getDevDependencies :: PackageJson -> [Dependency]
getDevDependencies packageJson = D.fromList $ M.toList $ devDependencies packageJson

type DependenciesMap = Map PackageName PackageVersion

type PackageName = String

type PackageVersion = String

instance FromJSON PackageJson
