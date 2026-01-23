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
    mergeWaspAndUserDeps,
  )
where

import Control.Monad (forM_)
import Data.Aeson
import Data.List (intercalate, sort)
import qualified Data.Map as Map
import GHC.Generics
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.Npm.Dependency as D
import qualified Wasp.ExternalConfig.Npm.PackageJson as PJ
import Wasp.Generator.Monad (Generator, GeneratorWarning (..), logGeneratorWarning)

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

-- | Ensures there are no conflicts between Wasp's dependencies and user's dependencies,
-- while allowing overrides for packages listed in overriddenDepNames.
-- For overridden packages, user's version is used instead of Wasp's, and a warning is emitted.
mergeWaspAndUserDeps :: [PJ.PackageName] -> NpmDepsFromWasp -> NpmDepsFromUser -> Generator NpmDepsForPackage
mergeWaspAndUserDeps overriddenDepNames waspDeps userDeps = do
  -- Emit warnings for each override
  forM_ overriddenDepNames $ \pkgName ->
    logGeneratorWarning $
      GenericGeneratorWarning $
        "Dependency override active for \""
          ++ pkgName
          ++ "\". You are using an unsupported version. "
          ++ "Wasp cannot guarantee compatibility."
  -- Return merged dependencies with user versions for overridden packages
  return $ mergeWithUserOverrides overriddenDepNames waspDeps userDeps

-- | Merges Wasp dependencies with user dependencies, using user's versions
-- for packages listed in overriddenDepNames.
mergeWithUserOverrides :: [PJ.PackageName] -> NpmDepsFromWasp -> NpmDepsFromUser -> NpmDepsForPackage
mergeWithUserOverrides overriddenDepNames (NpmDepsFromWasp waspPkg) (NpmDepsFromUser userPkg) =
  NpmDepsForPackage
    { dependencies = mergeDeps (dependencies waspPkg) (dependencies userPkg),
      devDependencies = mergeDeps (devDependencies waspPkg) (devDependencies userPkg),
      peerDependencies = []
    }
  where
    mergeDeps :: [D.Dependency] -> [D.Dependency] -> [D.Dependency]
    mergeDeps waspDeps userDeps =
      let waspMap = makeDepsByName waspDeps
          userMap = makeDepsByName userDeps
          -- For overridden deps, replace Wasp's version with user's version
          mergedMap =
            foldr
              ( \pkgName m -> case Map.lookup pkgName userMap of
                  Just userDep | pkgName `elem` overriddenDepNames -> Map.insert pkgName userDep m
                  _ -> m
              )
              waspMap
              overriddenDepNames
       in Map.elems mergedMap

buildWaspFrameworkNpmDeps :: AppSpec -> NpmDepsFromWasp -> NpmDepsFromWasp -> NpmDepsForFramework
buildWaspFrameworkNpmDeps spec fromServer fromWebApp =
  NpmDepsForFramework
    { npmDepsForServer = mergeWithUserOverrides overriddenDepNames fromServer userDeps,
      npmDepsForWebApp = mergeWithUserOverrides overriddenDepNames fromWebApp userDeps
    }
  where
    overriddenDepNames = D.name <$> PJ.getOverriddenDeps (AS.packageJson spec)
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

type DepsByName = Map.Map String D.Dependency

makeDepsByName :: [D.Dependency] -> DepsByName
makeDepsByName = Map.fromList . fmap (\d -> (D.name d, d))

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
