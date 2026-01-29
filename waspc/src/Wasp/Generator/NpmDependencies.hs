{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.Generator.NpmDependencies
  ( getDependenciesPackageJsonEntry,
    getDevDependenciesPackageJsonEntry,
    getPeerDependenciesPackageJsonEntry,
    getUserNpmDepsForPackage,
    NpmDepsForPackage (..),
    NpmDepsFromWasp (..),
    NpmDepsFromUser (..),
    buildWaspServerNpmDeps,
    getDependencyOverridesPackageJsonEntry,
    mergeWaspAndUserDeps,
  )
where

import Data.Aeson
import Data.List (intercalate, sort)
import GHC.Generics
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.Npm.Dependency as D
import qualified Wasp.ExternalConfig.Npm.PackageJson as PJ
import Wasp.Generator.Monad (Generator)

data NpmDepsForPackage = NpmDepsForPackage
  { dependencies :: [D.Dependency],
    devDependencies :: [D.Dependency],
    peerDependencies :: [D.Dependency]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype NpmDepsFromWasp = NpmDepsFromWasp {fromWasp :: NpmDepsForPackage}
  deriving (Show)

newtype NpmDepsFromUser = NpmDepsFromUser {fromUser :: NpmDepsForPackage}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

mergeWaspAndUserDeps :: NpmDepsFromWasp -> NpmDepsFromUser -> Generator NpmDepsForPackage
mergeWaspAndUserDeps waspDeps _userDeps = return $ waspDepsToPackageDeps waspDeps

buildWaspServerNpmDeps :: NpmDepsFromWasp -> NpmDepsForPackage
buildWaspServerNpmDeps = waspDepsToPackageDeps

getUserNpmDepsForPackage :: AppSpec -> NpmDepsFromUser
getUserNpmDepsForPackage spec =
  NpmDepsFromUser $
    NpmDepsForPackage
      { dependencies = PJ.getDependencies $ AS.packageJson spec,
        -- Should we allow user devDependencies? https://github.com/wasp-lang/wasp/issues/456
        devDependencies = PJ.getDevDependencies $ AS.packageJson spec,
        peerDependencies = []
      }

-- NpmDepsForPackage are equal if their sorted dependencies
-- are equal.
instance Eq NpmDepsForPackage where
  (==) a b = sortedDependencies a == sortedDependencies b

sortedDependencies :: NpmDepsForPackage -> ([D.Dependency], [D.Dependency])
sortedDependencies a = (sort $ dependencies a, sort $ devDependencies a)

waspDepsToPackageDeps :: NpmDepsFromWasp -> NpmDepsForPackage
waspDepsToPackageDeps npmDepsFromWasp =
  NpmDepsForPackage
    { dependencies = dependencies $ fromWasp npmDepsFromWasp,
      devDependencies = devDependencies $ fromWasp npmDepsFromWasp,
      -- Wasp dependencies are used for generating standalone applications, not libraries. They are
      -- not consumed by another package that could provide peer dependencies. Thus, peer
      -- dependencies are always empty.
      peerDependencies = []
    }

getDependenciesPackageJsonEntry :: NpmDepsForPackage -> String
getDependenciesPackageJsonEntry = dependenciesToPackageJsonEntryWithKey "dependencies" . dependencies

getDevDependenciesPackageJsonEntry :: NpmDepsForPackage -> String
getDevDependenciesPackageJsonEntry = dependenciesToPackageJsonEntryWithKey "devDependencies" . devDependencies

getPeerDependenciesPackageJsonEntry :: NpmDepsForPackage -> String
getPeerDependenciesPackageJsonEntry = dependenciesToPackageJsonEntryWithKey "peerDependencies" . peerDependencies

getDependencyOverridesPackageJsonEntry :: [D.Dependency] -> String
getDependencyOverridesPackageJsonEntry = dependenciesToPackageJsonEntryWithKey "overrides"

dependenciesToPackageJsonEntryWithKey :: String -> [D.Dependency] -> String
dependenciesToPackageJsonEntryWithKey key deps =
  "\""
    ++ key
    ++ "\": {"
    ++ intercalate ",\n  " (map (\dep -> "\"" ++ D.name dep ++ "\": \"" ++ D.version dep ++ "\"") deps)
    ++ "\n}"
