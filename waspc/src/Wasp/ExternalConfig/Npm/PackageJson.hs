{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.ExternalConfig.Npm.PackageJson
  ( PackageJson (..),
    WaspConfig (..),
    DependenciesMap,
    PackageName,
    PackageVersion,
    getDependencies,
    getDevDependencies,
    getOverriddenDeps,
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
    workspaces :: !(Maybe [String]),
    wasp :: !(Maybe WaspConfig)
  }
  deriving (Show, Generic, FromJSON)

data WaspConfig = WaspConfig
  { overriddenDeps :: !(Maybe DependenciesMap)
  }
  deriving (Show, Generic, FromJSON)

type DependenciesMap = Map PackageName PackageVersion

type PackageName = String

type PackageVersion = String

getDependencies :: PackageJson -> [Dependency]
getDependencies packageJson = D.fromList $ M.toList $ dependencies packageJson

getDevDependencies :: PackageJson -> [Dependency]
getDevDependencies packageJson = D.fromList $ M.toList $ devDependencies packageJson

-- | (The version is what Wasp requires, not what the user is using.)
getOverriddenDeps :: PackageJson -> [Dependency]
getOverriddenDeps pkgJson =
  maybe [] (D.fromList . M.toList) $ overriddenDeps =<< wasp pkgJson
