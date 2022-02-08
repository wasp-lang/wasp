module Wasp.Generator.PackageJsonGenerator
  ( DependencyConflictError (..),
    getDependenciesPackageJsonEntry,
    getDevDependenciesPackageJsonEntry,
    combinePackageJsonDependencies,
    PackageJsonDependencies (..),
    PackageJsonDependenciesError (..),
    conflictErrorToMessage,
    genPackageJsonDependencies,
  )
where

import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Maybe as Maybe
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Dependency as D
import Wasp.Generator.Monad (Generator, GeneratorError (..), logAndThrowGeneratorError)

data PackageJsonDependencies = PackageJsonDependencies
  { dependencies :: [D.Dependency],
    devDependencies :: [D.Dependency]
  }
  deriving (Show, Eq)

data PackageJsonDependenciesError = PackageJsonDependenciesError
  { dependenciesConflictErrors :: [DependencyConflictError],
    devDependenciesConflictErrors :: [DependencyConflictError]
  }
  deriving (Show, Eq)

data DependencyConflictError = DependencyConflictError
  { waspDependency :: D.Dependency,
    userDependency :: D.Dependency
  }
  deriving (Show, Eq)

-- | Generate a PackageJsonDependencies by combining wask dependencies with user dependencies
--   derived from AppSpec, or if there are conflicts, fail with error messages.
genPackageJsonDependencies :: AppSpec -> PackageJsonDependencies -> Generator PackageJsonDependencies
genPackageJsonDependencies spec waspDependencies =
  case combinePackageJsonDependencies waspDependencies userDependencies of
    Right deps -> return deps
    Left conflictErrorDeps ->
      logAndThrowGeneratorError $
        GenericGeneratorError $
          intercalate " ; " $
            map
              conflictErrorToMessage
              ( dependenciesConflictErrors conflictErrorDeps
                  ++ devDependenciesConflictErrors conflictErrorDeps
              )
  where
    userDependencies =
      PackageJsonDependencies
        { dependencies = fromMaybe [] $ AS.App.dependencies $ snd $ AS.getApp spec,
          -- Should we allow user devDependencies? https://github.com/wasp-lang/wasp/issues/456
          devDependencies = []
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

-- | Takes wasp npm dependencies and user npm dependencies and figures out how
--   to combine them together, returning (Right) a new PackageJsonDependencies
--   that combines them, and on error (Left), returns a PackageJsonDependenciesError
--   which describes which dependencies are in conflict.
combinePackageJsonDependencies :: PackageJsonDependencies -> PackageJsonDependencies -> Either PackageJsonDependenciesError PackageJsonDependencies
combinePackageJsonDependencies waspDependencies userDependencies =
  if null conflictErrors && null devConflictErrors
    then
      Right $
        PackageJsonDependencies
          { dependencies = dependencies waspDependencies ++ remainingUserDeps,
            devDependencies = devDependencies waspDependencies ++ remainingUserDevDeps
          }
    else
      Left $
        PackageJsonDependenciesError
          { dependenciesConflictErrors = conflictErrors,
            devDependenciesConflictErrors = devConflictErrors
          }
  where
    waspDepsByName = makeDepsByName $ dependencies waspDependencies
    waspDevDepsByName = makeDepsByName $ devDependencies waspDependencies
    userDepsByName = makeDepsByName $ dependencies userDependencies
    userDevDepsByName = makeDepsByName $ devDependencies userDependencies
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
getDependenciesPackageJsonEntry :: PackageJsonDependencies -> String
getDependenciesPackageJsonEntry = dependenciesToPackageJsonEntryWithKey "dependencies" . dependencies

-- | Construct devDependencies entry in package.json
getDevDependenciesPackageJsonEntry :: PackageJsonDependencies -> String
getDevDependenciesPackageJsonEntry = dependenciesToPackageJsonEntryWithKey "devDependencies" . devDependencies

dependenciesToPackageJsonEntryWithKey :: String -> [D.Dependency] -> String
dependenciesToPackageJsonEntryWithKey key deps =
  "\""
    ++ key
    ++ "\": {"
    ++ intercalate ",\n  " (map (\dep -> "\"" ++ D.name dep ++ "\": \"" ++ D.version dep ++ "\"") deps)
    ++ "\n}"