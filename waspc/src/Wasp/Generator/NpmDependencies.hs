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
import qualified Data.Map as Map
import Data.Maybe (isJust)
import GHC.Generics
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.Npm.Dependency as D
import qualified Wasp.ExternalConfig.Npm.PackageJson as PJ
import Wasp.Util (zipMaps)

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

buildWaspServerNpmDeps :: AppSpec -> NpmDepsFromWasp -> NpmDepsForPackage
buildWaspServerNpmDeps spec fromServer = mergeWaspAndUserDeps fromServer userDeps
  where
    userDeps = getUserNpmDepsForPackage spec

-- | Merges Wasp dependencies with user dependencies. When a package appears in
-- both, the user's version takes precedence. User-only dependencies don't need
-- to appear in the generated project's dependencies, so we don't regard them
-- here.
mergeWaspAndUserDeps :: NpmDepsFromWasp -> NpmDepsFromUser -> NpmDepsForPackage
mergeWaspAndUserDeps (NpmDepsFromWasp waspPkg) (NpmDepsFromUser userPkg) =
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
