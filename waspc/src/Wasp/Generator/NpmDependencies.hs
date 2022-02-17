{-# LANGUAGE DeriveGeneric #-}

module Wasp.Generator.NpmDependencies
  ( DependencyConflictError (..),
    getDependenciesPackageJsonEntry,
    getDevDependenciesPackageJsonEntry,
    combineNpmDepsForPackage,
    NpmDepsForPackage (..),
    NpmDepsForPackageError (..),
    conflictErrorToMessage,
    genNpmDepsForPackage,
    NpmDepsForFullStack,
    NpmDepsForWasp (..),
    NpmDepsForUser (..),
    buildNpmDepsForFullStack,
  )
where

import Data.Aeson
import Data.List (intercalate, sort)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Maybe as Maybe
import GHC.Generics
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Dependency as D
import Wasp.Generator.Monad (Generator, GeneratorError (..), logAndThrowGeneratorError)

data NpmDepsForFullStack = NpmDepsForFullStack
  { npmDepsForServer :: NpmDepsForPackage,
    npmDepsForWebApp :: NpmDepsForPackage
  }
  deriving (Show, Eq, Generic)

instance ToJSON NpmDepsForFullStack where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON NpmDepsForFullStack

data NpmDepsForPackage = NpmDepsForPackage
  { dependencies :: [D.Dependency],
    devDependencies :: [D.Dependency]
  }
  deriving (Show, Generic)

data NpmDepsForWasp = NpmDepsForWasp
  { waspDependencies :: [D.Dependency],
    waspDevDependencies :: [D.Dependency]
  }
  deriving (Show)

data NpmDepsForUser = NpmDepsForUser
  { userDependencies :: [D.Dependency],
    userDevDependencies :: [D.Dependency]
  }
  deriving (Show)

instance ToJSON NpmDepsForPackage where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON NpmDepsForPackage

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

buildNpmDepsForFullStack :: AppSpec -> NpmDepsForWasp -> NpmDepsForWasp -> Either String NpmDepsForFullStack
buildNpmDepsForFullStack spec forServer forWebApp =
  case (combinedServerDeps, combinedWebAppDeps) of
    (Right a, Right b) ->
      Right
        NpmDepsForFullStack
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
    { userDependencies = fromMaybe [] $ AS.App.dependencies $ snd $ AS.getApp spec,
      -- Should we allow user devDependencies? https://github.com/wasp-lang/wasp/issues/456
      userDevDependencies = []
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
combineNpmDepsForPackage :: NpmDepsForWasp -> NpmDepsForUser -> Either NpmDepsForPackageError NpmDepsForPackage
combineNpmDepsForPackage npmDepsForWasp npmDepsForUser =
  if null conflictErrors && null devConflictErrors
    then
      Right $
        NpmDepsForPackage
          { dependencies = waspDependencies npmDepsForWasp ++ remainingUserDeps,
            devDependencies = waspDevDependencies npmDepsForWasp ++ remainingUserDevDeps
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
    remainingUserDeps = getRemainingUserDeps allWaspDepsByName userDepsByName
    remainingUserDevDeps = getRemainingUserDeps allWaspDepsByName userDevDepsByName

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

-- Given a map of wasp dependencies and a map of user dependencies, construct a
-- a list of user dependencies that remain once any overlapping wasp dependencies
-- have been removed. This assumes conflict detection was already passed.
getRemainingUserDeps :: DepsByName -> DepsByName -> [D.Dependency]
getRemainingUserDeps waspDepsByName userDepsByName = Map.elems $ userDepsByName `Map.difference` waspDepsByName

-- Construct a map of dependency keyed by dependency name.
makeDepsByName :: [D.Dependency] -> DepsByName
makeDepsByName = Map.fromList . fmap (\d -> (D.name d, d))

-- | Construct dependencies entry in package.json
getDependenciesPackageJsonEntry :: NpmDepsForPackage -> String
getDependenciesPackageJsonEntry = dependenciesToPackageJsonEntryWithKey "dependencies" . dependencies

-- | Construct devDependencies entry in package.json
getDevDependenciesPackageJsonEntry :: NpmDepsForPackage -> String
getDevDependenciesPackageJsonEntry = dependenciesToPackageJsonEntryWithKey "devDependencies" . devDependencies

dependenciesToPackageJsonEntryWithKey :: String -> [D.Dependency] -> String
dependenciesToPackageJsonEntryWithKey key deps =
  "\""
    ++ key
    ++ "\": {"
    ++ intercalate ",\n  " (map (\dep -> "\"" ++ D.name dep ++ "\": \"" ++ D.version dep ++ "\"") deps)
    ++ "\n}"
