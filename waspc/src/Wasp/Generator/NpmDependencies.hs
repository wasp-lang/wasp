{-# LANGUAGE DeriveGeneric #-}

module Wasp.Generator.NpmDependencies
  ( DependencyConflictError (..),
    getDependenciesPackageJsonEntry,
    getDevDependenciesPackageJsonEntry,
    getPeerDependenciesPackageJsonEntry,
    getUserNpmDepsForPackage,
    getNpmDepsConflicts,
    NpmDepsForPackage (..),
    NpmDepsForPackageError (..),
    conflictErrorToMessage,
    genNpmDepsForPackage,
    NpmDepsForFramework,
    NpmDepsFromWasp (..),
    NpmDepsFromUser (..),
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

newtype NpmDepsFromWasp = NpmDepsFromWasp {fromWasp :: NpmDepsForPackage}
  deriving (Show)

data NpmDepsFromUser = NpmDepsFromUser {fromUser :: NpmDepsForPackage}
  deriving (Show, Eq, Generic)

instance ToJSON NpmDepsFromUser

instance FromJSON NpmDepsFromUser

data NpmDepsForPackageError = NpmDepsForPackageError
  { dependenciesConflictErrors :: [DependencyConflictError],
    devDependenciesConflictErrors :: [DependencyConflictError]
  }
  deriving (Show, Eq)

data DependencyConflictError = DependencyConflictError
  { waspDependency :: D.Dependency,
    userDependency :: D.Dependency
  }
  deriving (Show, Eq)

-- | Generate a NpmDepsForPackage by combining wasp dependencies with user dependencies
--   derived from AppSpec, or if there are conflicts, fail with error messages.
genNpmDepsForPackage :: AppSpec -> NpmDepsFromWasp -> Generator NpmDepsForPackage
genNpmDepsForPackage spec npmDepsFromWasp =
  case getNpmDepsConflicts npmDepsFromWasp (getUserNpmDepsForPackage spec) of
    Nothing -> return $ waspDepsToPackageDeps npmDepsFromWasp
    Just conflictErrorDeps ->
      logAndThrowGeneratorError $
        GenericGeneratorError $
          intercalate "\n " $
            map
              conflictErrorToMessage
              ( dependenciesConflictErrors conflictErrorDeps
                  ++ devDependenciesConflictErrors conflictErrorDeps
              )

buildWaspFrameworkNpmDeps :: AppSpec -> NpmDepsFromWasp -> NpmDepsFromWasp -> Either String NpmDepsForFramework
buildWaspFrameworkNpmDeps spec forServer forWebApp =
  case (serverDepConflicts, webAppDepConflicts) of
    (Nothing, Nothing) ->
      Right
        NpmDepsForFramework
          { npmDepsForServer = waspDepsToPackageDeps forServer,
            npmDepsForWebApp = waspDepsToPackageDeps forWebApp
          }
    _ -> Left "Could not construct npm dependencies due to a previously reported conflict."
  where
    userDeps = getUserNpmDepsForPackage spec
    serverDepConflicts = getNpmDepsConflicts forServer userDeps
    webAppDepConflicts = getNpmDepsConflicts forWebApp userDeps

getUserNpmDepsForPackage :: AppSpec -> NpmDepsFromUser
getUserNpmDepsForPackage spec =
  NpmDepsFromUser $
    NpmDepsForPackage
      { dependencies = PJ.getDependencies $ AS.packageJson spec,
        -- Should we allow user devDependencies? https://github.com/wasp-lang/wasp/issues/456
        devDependencies = PJ.getDevDependencies $ AS.packageJson spec,
        peerDependencies = []
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

-- | Checks the user's dependencies compatibility against Wasp's declared npm dependencies.
getNpmDepsConflicts :: NpmDepsFromWasp -> NpmDepsFromUser -> Maybe NpmDepsForPackageError
getNpmDepsConflicts npmDepsFromWasp npmDepsFromUser =
  if null conflictErrors && null devConflictErrors
    then Nothing
    else
      Just $
        NpmDepsForPackageError
          { dependenciesConflictErrors = conflictErrors,
            devDependenciesConflictErrors = devConflictErrors
          }
  where
    waspDeps = dependencies $ fromWasp npmDepsFromWasp
    waspDevDeps = devDependencies $ fromWasp npmDepsFromWasp
    allWaspDepsByName = (Map.union `on` makeDepsByName) waspDeps waspDevDeps

    userDepsByName = makeDepsByName $ dependencies $ fromUser npmDepsFromUser
    userDevDepsByName = makeDepsByName $ devDependencies $ fromUser npmDepsFromUser

    conflictErrors = determineConflictErrors allWaspDepsByName userDepsByName
    devConflictErrors = determineConflictErrors allWaspDepsByName userDevDepsByName

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
