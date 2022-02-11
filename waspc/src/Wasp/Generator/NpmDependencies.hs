{-# LANGUAGE DeriveGeneric #-}

module Wasp.Generator.NpmDependencies
  ( DependencyConflictError (..),
    getDependenciesPackageJsonEntry,
    getDevDependenciesPackageJsonEntry,
    combineNpmDependencies,
    NpmDependencies (..),
    NpmDependenciesError (..),
    conflictErrorToMessage,
    genNpmDependencies,
    FullStackNpmDependencies,
    buildFullStackNpmDependencies,
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

data FullStackNpmDependencies = FullStackNpmDependencies
  { serverDependencies :: NpmDependencies,
    webAppDependencies :: NpmDependencies
  }
  deriving (Show, Eq, Generic)

instance ToJSON FullStackNpmDependencies where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON FullStackNpmDependencies

data NpmDependencies = NpmDependencies
  { dependencies :: [D.Dependency],
    devDependencies :: [D.Dependency]
  }
  deriving (Show, Generic)

instance ToJSON NpmDependencies where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON NpmDependencies

data NpmDependenciesError = NpmDependenciesError
  { dependenciesConflictErrors :: [DependencyConflictError],
    devDependenciesConflictErrors :: [DependencyConflictError]
  }
  deriving (Show, Eq)

data DependencyConflictError = DependencyConflictError
  { waspDependency :: D.Dependency,
    userDependency :: D.Dependency
  }
  deriving (Show, Eq)

-- | Generate a NpmDependencies by combining wasp dependencies with user dependencies
--   derived from AppSpec, or if there are conflicts, fail with error messages.
genNpmDependencies :: AppSpec -> NpmDependencies -> Generator NpmDependencies
genNpmDependencies spec waspDependencies =
  case combineNpmDependencies waspDependencies (getUserNpmDependencies spec) of
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

buildFullStackNpmDependencies :: AppSpec -> NpmDependencies -> NpmDependencies -> Either String FullStackNpmDependencies
buildFullStackNpmDependencies spec serverWaspDependencies webAppWaspDependencies =
  case (combinedServerDependencies, combinedWebAppDependencies) of
    (Right a, Right b) ->
      Right
        FullStackNpmDependencies
          { serverDependencies = a,
            webAppDependencies = b
          }
    _ -> Left "Could not construct npm dependencies due to a previously reported conflict."
  where
    userDependencies = getUserNpmDependencies spec
    combinedServerDependencies = combineNpmDependencies serverWaspDependencies userDependencies
    combinedWebAppDependencies = combineNpmDependencies webAppWaspDependencies userDependencies

getUserNpmDependencies :: AppSpec -> NpmDependencies
getUserNpmDependencies spec =
  NpmDependencies
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

-- NpmDependencies are equal if their sorted dependencies
-- are equal.
instance Eq NpmDependencies where
  (==) a b = sortedDependencies a == sortedDependencies b

sortedDependencies :: NpmDependencies -> ([D.Dependency], [D.Dependency])
sortedDependencies a = (sort $ dependencies a, sort $ devDependencies a)

-- | Takes wasp npm dependencies and user npm dependencies and figures out how
--   to combine them together, returning (Right) a new NpmDependencies
--   that combines them, and on error (Left), returns a NpmDependenciesError
--   which describes which dependencies are in conflict.
combineNpmDependencies :: NpmDependencies -> NpmDependencies -> Either NpmDependenciesError NpmDependencies
combineNpmDependencies waspDependencies userDependencies =
  if null conflictErrors && null devConflictErrors
    then
      Right $
        NpmDependencies
          { dependencies = dependencies waspDependencies ++ remainingUserDeps,
            devDependencies = devDependencies waspDependencies ++ remainingUserDevDeps
          }
    else
      Left $
        NpmDependenciesError
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
getDependenciesPackageJsonEntry :: NpmDependencies -> String
getDependenciesPackageJsonEntry = dependenciesToPackageJsonEntryWithKey "dependencies" . dependencies

-- | Construct devDependencies entry in package.json
getDevDependenciesPackageJsonEntry :: NpmDependencies -> String
getDevDependenciesPackageJsonEntry = dependenciesToPackageJsonEntryWithKey "devDependencies" . devDependencies

dependenciesToPackageJsonEntryWithKey :: String -> [D.Dependency] -> String
dependenciesToPackageJsonEntryWithKey key deps =
  "\""
    ++ key
    ++ "\": {"
    ++ intercalate ",\n  " (map (\dep -> "\"" ++ D.name dep ++ "\": \"" ++ D.version dep ++ "\"") deps)
    ++ "\n}"