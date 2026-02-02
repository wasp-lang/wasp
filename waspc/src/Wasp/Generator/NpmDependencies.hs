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
    mergeWaspAndUserDeps,
    mergeDepsWith,
    empty,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, ToJSON)
import Data.List (intercalate, sort)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import GHC.Generics (Generic)
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

empty :: NpmDepsForPackage
empty = NpmDepsForPackage [] [] []

newtype NpmDepsFromWasp = NpmDepsFromWasp {fromWasp :: NpmDepsForPackage}
  deriving (Show)

newtype NpmDepsFromUser = NpmDepsFromUser {fromUser :: NpmDepsForPackage}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Merges Wasp dependencies with user dependencies. When a package appears in
-- both, the user's version takes precedence. User-only dependencies are not
-- included here (they are added separately via the package.json overrides field).
mergeWaspAndUserDeps :: NpmDepsFromWasp -> NpmDepsFromUser -> NpmDepsForPackage
mergeWaspAndUserDeps (NpmDepsFromWasp waspPkg) (NpmDepsFromUser userPkg) =
  mergeDepsWith merge waspPkg userPkg
  where
    -- If it's in both, prefer `userDep`. Otherwise, take `waspDep`. (We don't
    -- to want add a dependency that is only in userDeps here, only override
    -- existing ones.)
    merge _pkgName waspDep userDep
      | isJust waspDep && isJust userDep = userDep
      | otherwise = waspDep

mergeDepsWith :: (String -> Maybe String -> Maybe String -> Maybe String) -> NpmDepsForPackage -> NpmDepsForPackage -> NpmDepsForPackage
mergeDepsWith mergeFunc a b =
  NpmDepsForPackage
    { dependencies = mergeOn dependencies,
      devDependencies = mergeOn devDependencies,
      peerDependencies = mergeOn peerDependencies
    }
  where
    mergeOn field = fromMap $ zipMaps mergeFunc (toMap $ field a) (toMap $ field b)

    toMap = Map.fromList . map D.toPair
    fromMap = D.fromList . Map.toList

newtype ErrorOnIncompatible = ErrorOnIncompatible {getNpmDepsForPackage :: NpmDepsForPackage}

instance Semigroup ErrorOnIncompatible where
  a <> b = ErrorOnIncompatible $ mergeDepsWith mergeFunc (getNpmDepsForPackage a) (getNpmDepsForPackage b)
    where
      mergeFunc pkgName (Just waspVersion) (Just userVersion)
        | waspVersion /= userVersion =
            error $ "Incompatible versions for package " ++ pkgName ++ ": " ++ waspVersion ++ " vs " ++ userVersion ++ "."
      mergeFunc _ x y = x <|> y

instance Monoid ErrorOnIncompatible where
  mempty = ErrorOnIncompatible empty

buildWaspFrameworkNpmDeps :: AppSpec -> NpmDepsFromWasp -> NpmDepsFromWasp -> NpmDepsForFramework
buildWaspFrameworkNpmDeps spec fromServer fromWebApp =
  NpmDepsForFramework
    { npmDepsForServer = mergeWaspAndUserDeps fromServer userDeps,
      npmDepsForWebApp = mergeWaspAndUserDeps fromWebApp userDeps
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

dependenciesToPackageJsonEntryWithKey :: String -> [D.Dependency] -> String
dependenciesToPackageJsonEntryWithKey key deps =
  "\""
    ++ key
    ++ "\": {"
    ++ intercalate ",\n  " (map (\dep -> "\"" ++ D.name dep ++ "\": \"" ++ D.version dep ++ "\"") deps)
    ++ "\n}"
