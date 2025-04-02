{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.ExternalConfig.Npm
  ( PackageJson (..),
    DependenciesMap,
    PackageName,
    PackageVersion,
    getDependencies,
    getDevDependencies,
  )
where

import Data.Aeson
import Data.Data (Data)
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics
import Wasp.ExternalConfig.Npm.Dependency (Dependency)
import qualified Wasp.ExternalConfig.Npm.Dependency as D

data Dependency = Dependency
  { name :: String,
    -- | NOTE: By npm docs, this can be semver version range,
    -- but it can also be a URL (tarball, git or Github), or a local file path.
    version :: String
  }
  deriving (Show, Eq, Data, Generic)

instance ToJSON Dependency where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Dependency

instance Ord Dependency where
  a <= b = name a <= name b

fromList :: [(String, String)] -> [Dependency]
fromList = map make

make :: (String, String) -> Dependency
make (n, v) = Dependency {name = n, version = v}

data PackageJson = PackageJson
  { name :: !String,
    dependencies :: !DependenciesMap,
    devDependencies :: !DependenciesMap
  }
  deriving (Show, Generic, FromJSON)

getDependencies :: PackageJson -> [Dependency]
getDependencies packageJson = D.fromList $ M.toList $ dependencies packageJson

getDevDependencies :: PackageJson -> [Dependency]
getDevDependencies packageJson = D.fromList $ M.toList $ devDependencies packageJson

type DependenciesMap = Map PackageName PackageVersion

type PackageName = String

type PackageVersion = String
