{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.Generator.NpmDependencies
  ( getDependenciesPackageJsonEntry,
    getDevDependenciesPackageJsonEntry,
    getPeerDependenciesPackageJsonEntry,
    getUserNpmDepsForPackage,
    NpmDepsForPackage (..),
    NpmDepsForFramework,
    NpmDepsFromWasp (..),
    NpmDepsFromUser (..),
    buildWaspFrameworkNpmDeps,
    getDependencyOverridesPackageJsonEntry,
    mergeWithUserOverrides,
  )
where

import Data.Aeson
import Data.List (intercalate, sort)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import GHC.Generics
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.Npm.Dependency as D
import qualified Wasp.ExternalConfig.Npm.PackageJson as PJ
import Wasp.Util (zipMaps)

data NpmDepsForFramework = NpmDepsForFramework
  { npmDepsForServer :: NpmDepsForPackage,
    npmDepsForWebApp :: NpmDepsForPackage
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

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

-- | Merges Wasp dependencies with user dependencies, using user's versions for
-- packages listed in overriddenDepNames.
mergeWithUserOverrides :: NpmDepsFromWasp -> NpmDepsFromUser -> NpmDepsForPackage
mergeWithUserOverrides (NpmDepsFromWasp waspPkg) (NpmDepsFromUser userPkg) =
  NpmDepsForPackage
    { dependencies = mergeDeps (dependencies waspPkg) allUserDependencies,
      devDependencies = mergeDeps (devDependencies waspPkg) allUserDependencies,
      peerDependencies = []
    }
  where
    allUserDependencies = dependencies userPkg ++ devDependencies userPkg

    mergeDeps :: [D.Dependency] -> [D.Dependency] -> [D.Dependency]
    mergeDeps waspDeps userDeps =
      D.fromList $ Map.toList $ zipMaps (const merge) waspMap userMap
      where
        -- If it's in both, prefer `userDep`. Otherwise, take `waspDep`. (We
        -- don't to want add a dependency that is only in userDeps here, only
        -- override existing ones.)
        merge waspDep userDep
          | isJust waspDep && isJust userDep = userDep
          | otherwise = waspDep

        waspMap = Map.fromList $ D.toPair <$> waspDeps
        userMap = Map.fromList $ D.toPair <$> userDeps

buildWaspFrameworkNpmDeps :: AppSpec -> NpmDepsFromWasp -> NpmDepsFromWasp -> NpmDepsForFramework
buildWaspFrameworkNpmDeps spec fromServer fromWebApp =
  NpmDepsForFramework
    { npmDepsForServer = mergeWithUserOverrides fromServer userDeps,
      npmDepsForWebApp = mergeWithUserOverrides fromWebApp userDeps
    }
  where
    userDeps = getUserNpmDepsForPackage spec

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
