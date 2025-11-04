{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.ExternalConfig.Npm.PackageJson
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
import Wasp.ExternalConfig.Npm.Dependency (Dependency)
import qualified Wasp.ExternalConfig.Npm.Dependency as D

data PackageJson = PackageJson
  { name :: !String,
    dependencies :: !DependenciesMap,
    devDependencies :: !DependenciesMap,
    workspaces :: !(Maybe [String])
  }
  deriving (Show, Generic, FromJSON)

getDependencies :: PackageJson -> [Dependency]
getDependencies packageJson = D.fromList $ M.toList $ dependencies packageJson

getDevDependencies :: PackageJson -> [Dependency]
getDevDependencies packageJson = D.fromList $ M.toList $ devDependencies packageJson

type DependenciesMap = Map PackageName PackageVersion

type PackageName = String

type PackageVersion = String
