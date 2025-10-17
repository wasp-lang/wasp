{-# LANGUAGE DeriveGeneric #-}

module Wasp.Generator.NpmDependencies
  ( DependencyConflictError (..),
    getDependenciesPackageJsonEntry,
    getDevDependenciesPackageJsonEntry,
    getPeerDependenciesPackageJsonEntry,
    getUserNpmDepsForPackage,
    getNpmDepsConflicts,
    NpmDepsForPackage (..),
    conflictErrorToMessage,
    genNpmDepsForPackage,
    NpmDepsForFramework,
    NpmDepsForWasp (..),
    NpmDepsForUser (..),
    buildWaspFrameworkNpmDeps,
    getDependencyOverridesPackageJsonEntry,
  )
where

import Data.Aeson
import Data.Function (on)
import Data.List (intercalate, sort)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import GHC.Generics
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.Npm.Dependency as D
import qualified Wasp.ExternalConfig.Npm.PackageJson as PJ
import Wasp.Generator.Monad (Generator, GeneratorError (..), logAndThrowGeneratorError)

data NpmDepsForFramework = NpmDepsForFramework
  { npmDepsForServer :: NpmDepsForPackage,
    npmDepsForWebApp :: NpmDepsForPackage
  }
  deriving (Show, Eq, Generic)

instance ToJSON NpmDepsForFramework

instance FromJSON NpmDepsForFramework

data NpmDepsForPackage = NpmDepsForPackage
  { dependencies :: [D.Dependency],
    devDependencies :: [D.Dependency],
    peerDependencies :: [D.Dependency]
  }
  deriving (Show, Generic)

instance ToJSON NpmDepsForPackage

instance FromJSON NpmDepsForPackage

data NpmDepsForWasp = NpmDepsForWasp
  { waspDependencies :: [D.Dependency],
    waspDevDependencies :: [D.Dependency]
  }
  deriving (Show)

data NpmDepsForUser = NpmDepsForUser
  { userDependencies :: [D.Dependency],
    userDevDependencies :: [D.Dependency]
  }
  deriving (Show, Eq, Generic)

instance ToJSON NpmDepsForUser

instance FromJSON NpmDepsForUser

data DependencyConflictError = DependencyConflictError
  { waspDependency :: D.Dependency,
    userDependency :: D.Dependency
  }
  deriving (Show, Eq)

-- | Generate a NpmDepsForPackage by combining wasp dependencies with user dependencies
--   derived from AppSpec, or if there are conflicts, fail with error messages.
genNpmDepsForPackage :: AppSpec -> NpmDepsForWasp -> Generator NpmDepsForPackage
genNpmDepsForPackage spec npmDepsForWasp
  | null conflictErrors = return $ waspDepsToPackageDeps npmDepsForWasp
  | otherwise =
      logAndThrowGeneratorError $
        GenericGeneratorError $
          intercalate "\n " $
            map
              conflictErrorToMessage
              conflictErrors
  where
    conflictErrors = getNpmDepsConflicts npmDepsForWasp (getUserNpmDepsForPackage spec)

buildWaspFrameworkNpmDeps :: AppSpec -> NpmDepsForWasp -> NpmDepsForWasp -> Either String NpmDepsForFramework
buildWaspFrameworkNpmDeps spec forServer forWebApp
  | hasConflicts = Left "Could not construct npm dependencies due to a previously reported conflict."
  | otherwise =
      Right $
        NpmDepsForFramework
          { npmDepsForServer = waspDepsToPackageDeps forServer,
            npmDepsForWebApp = waspDepsToPackageDeps forWebApp
          }
  where
    hasConflicts = not $ null serverDepConflicts && null webAppDepConflicts
    serverDepConflicts = getNpmDepsConflicts forServer userDeps
    webAppDepConflicts = getNpmDepsConflicts forWebApp userDeps
    userDeps = getUserNpmDepsForPackage spec

getUserNpmDepsForPackage :: AppSpec -> NpmDepsForUser
getUserNpmDepsForPackage spec =
  NpmDepsForUser
    { userDependencies = PJ.getDependencies $ AS.packageJson spec,
      -- Should we allow user devDependencies? https://github.com/wasp-lang/wasp/issues/456
      userDevDependencies = PJ.getDevDependencies $ AS.packageJson spec
    }

conflictErrorToMessage :: DependencyConflictError -> String
conflictErrorToMessage DependencyConflictError {waspDependency = waspDep, userDependency = userDep} =
  "Error: Dependency conflict for user dependency ("
    ++ D.name userDep
    ++ ", "
    ++ D.version userDep
    ++ "): "
    ++ "Version must be set to the exactly the same version as"
    ++ " the one wasp is using: "
    ++ D.version waspDep

-- NpmDepsForPackage are equal if their sorted dependencies
-- are equal.
instance Eq NpmDepsForPackage where
  (==) a b = sortedDependencies a == sortedDependencies b

sortedDependencies :: NpmDepsForPackage -> ([D.Dependency], [D.Dependency])
sortedDependencies a = (sort $ dependencies a, sort $ devDependencies a)

waspDepsToPackageDeps :: NpmDepsForWasp -> NpmDepsForPackage
waspDepsToPackageDeps npmDepsForWasp =
  NpmDepsForPackage
    { dependencies = waspDependencies npmDepsForWasp,
      devDependencies = waspDevDependencies npmDepsForWasp,
      -- Wasp dependencies are used for generating standalone applications, not libraries. They are
      -- not consumed by another package that could provide peer dependencies. Thus, peer
      -- dependencies are always empty.
      peerDependencies = []
    }

-- | Checks the user's dependencies compatibility against Wasp's declared npm dependencies.
getNpmDepsConflicts :: NpmDepsForWasp -> NpmDepsForUser -> [DependencyConflictError]
getNpmDepsConflicts npmDepsForWasp npmDepsForUser =
  conflictErrors ++ devConflictErrors
  where
    conflictErrors = determineConflictErrors allWaspDepsByName userDepsByName
    devConflictErrors = determineConflictErrors allWaspDepsByName userDevDepsByName

    allWaspDepsByName = (Map.union `on` makeDepsByName) waspDeps waspDevDeps
    waspDeps = waspDependencies npmDepsForWasp
    waspDevDeps = waspDevDependencies npmDepsForWasp

    userDepsByName = makeDepsByName $ userDependencies npmDepsForUser
    userDevDepsByName = makeDepsByName $ userDevDependencies npmDepsForUser

type DepsByName = Map.Map String D.Dependency

-- Given a map of wasp dependencies and a map of user dependencies, construct a
-- list of conflict errors for any dependencies that have a matching name but
-- different version.
determineConflictErrors :: DepsByName -> DepsByName -> [DependencyConflictError]
determineConflictErrors waspDepsByName userDepsByName =
  Maybe.mapMaybe makeConflictErrorIfMismatchedVersion (Map.toAscList overlappingDeps)
  where
    overlappingDeps = waspDepsByName `Map.intersection` userDepsByName
    makeConflictErrorIfMismatchedVersion :: (String, D.Dependency) -> Maybe DependencyConflictError
    makeConflictErrorIfMismatchedVersion (waspDepName, waspDep) = do
      userDep <- Map.lookup waspDepName userDepsByName
      if D.version waspDep /= D.version userDep
        then Just $ DependencyConflictError waspDep userDep
        else Nothing

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
