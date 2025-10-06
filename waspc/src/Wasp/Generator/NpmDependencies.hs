{-# LANGUAGE DeriveGeneric #-}

module Wasp.Generator.NpmDependencies
  ( DependencyConflictError (..),
    getDependenciesPackageJsonEntry,
    getDevDependenciesPackageJsonEntry,
    getPeerDependenciesPackageJsonEntry,
    getUserNpmDepsForPackage,
    combineNpmDepsForPackage,
    NpmDepsForPackage (..),
    NpmDepsForPackageError (..),
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
genNpmDepsForPackage :: AppSpec -> NpmDepsForWasp -> Generator NpmDepsForPackage
genNpmDepsForPackage spec npmDepsForWasp =
  case combineNpmDepsForPackage npmDepsForWasp (getUserNpmDepsForPackage spec) of
    Right deps -> return deps
    Left conflictErrorDeps ->
      logAndThrowGeneratorError $
        GenericGeneratorError $
          intercalate "\n " $
            map
              conflictErrorToMessage
              ( dependenciesConflictErrors conflictErrorDeps
                  ++ devDependenciesConflictErrors conflictErrorDeps
              )

buildWaspFrameworkNpmDeps :: AppSpec -> NpmDepsForWasp -> NpmDepsForWasp -> Either String NpmDepsForFramework
buildWaspFrameworkNpmDeps spec forServer forWebApp =
  case (combinedServerDeps, combinedWebAppDeps) of
    (Right a, Right b) ->
      Right
        NpmDepsForFramework
          { npmDepsForServer = a,
            npmDepsForWebApp = b
          }
    _ -> Left "Could not construct npm dependencies due to a previously reported conflict."
  where
    userDeps = getUserNpmDepsForPackage spec
    combinedServerDeps = combineNpmDepsForPackage forServer userDeps
    combinedWebAppDeps = combineNpmDepsForPackage forWebApp userDeps

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

-- | Takes wasp npm dependencies and user npm dependencies and figures out how
--   to combine them together, returning (Right) a new NpmDepsForPackage
--   that combines them, and on error (Left), returns a NpmDepsForPackageError
--   which describes which dependencies are in conflict.
-- TODO: The comment above and function name are not exactly correct any more,
-- as user deps don't get combined with the wasp deps any more, instead user deps
-- are just checked against wasp deps to see if there are any conflicts, and then
-- wasp deps are more or less returned as they are (maybe with some changes? But certainly no user deps added).
-- This function deserves rewriting / rethinking. This should be addressed while solving
-- GH issue https://github.com/wasp-lang/wasp/issues/1644 .
combineNpmDepsForPackage :: NpmDepsForWasp -> NpmDepsForUser -> Either NpmDepsForPackageError NpmDepsForPackage
combineNpmDepsForPackage npmDepsForWasp npmDepsForUser =
  if null conflictErrors && null devConflictErrors
    then
      Right $
        NpmDepsForPackage
          { dependencies = Map.elems remainingWapsDeps,
            devDependencies = Map.elems remainingWaspDevDeps,
            -- Peer dependencies are empty. The generated framework code is not intended to be
            -- consumed as a library by another package; instead, it is a standalone application, so
            -- there is no parent package to provide any peer dependencies.
            peerDependencies = []
          }
    else
      Left $
        NpmDepsForPackageError
          { dependenciesConflictErrors = conflictErrors,
            devDependenciesConflictErrors = devConflictErrors
          }
  where
    waspDepsByName = makeDepsByName $ waspDependencies npmDepsForWasp
    waspDevDepsByName = makeDepsByName $ waspDevDependencies npmDepsForWasp
    userDepsByName = makeDepsByName $ userDependencies npmDepsForUser
    userDevDepsByName = makeDepsByName $ userDevDependencies npmDepsForUser
    allWaspDepsByName = waspDepsByName `Map.union` waspDevDepsByName
    conflictErrors = determineConflictErrors allWaspDepsByName userDepsByName
    devConflictErrors = determineConflictErrors allWaspDepsByName userDevDepsByName
    remainingWapsDeps = waspDepsByName `Map.difference` userDepsByName
    remainingWaspDevDeps = waspDevDepsByName `Map.difference` userDevDepsByName

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
