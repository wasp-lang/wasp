module Wasp.Generator.PackageJsonGenerator
  ( resolveNpmDeps,
    resolveDependencies,
    DependencyConflictError (DependencyConflictError),
    npmDepsToPackageJsonEntry,
    npmDevDepsToPackageJsonEntry,
  )
where

import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Wasp.AppSpec.App.Dependency as D

type NpmDependenciesConflictError = String

data DependencyConflictError = DependencyConflictError D.Dependency D.Dependency
  deriving (Show, Eq)

-- | Takes wasp npm dependencies and user npm dependencies and figures out how
--   to combine them together, returning (Right) a list of npm dependencies to
--   be used on behalf of wasp and then also a list of npm dependencies to be
--   used on behalf of user. These lists might be the same as the initial ones,
--   but might also be different.

--   On error (Left), returns a list of conflicting wasp dep, user dep pairs

--   This is used in resolveNpmDeps to separate the error message generation
--   from its representation
resolveDependencies ::
  [D.Dependency] ->
  [D.Dependency] ->
  Either
    [DependencyConflictError]
    ([D.Dependency], [D.Dependency])
resolveDependencies waspDeps userDeps =
  if null conflicting
    then Right (waspDeps, createDeps userDepsNotInWaspDeps)
    else Left conflicting
  where
    waspDepsMap = createMap waspDeps
    userDepsMap = createMap userDeps
    overlappingDeps = waspDepsMap `Map.intersection` userDepsMap
    userDepsNotInWaspDeps = userDepsMap `Map.difference` waspDepsMap
    createMap :: [D.Dependency] -> Map.Map String String
    createMap deps = Map.fromList $ fmap toTuple deps
      where
        toTuple :: D.Dependency -> (String, String)
        toTuple dep = (D.name dep, D.version dep)
    createDeps :: Map.Map String String -> [D.Dependency]
    createDeps depsMap = D.create <$> Map.toList depsMap

    -- get all items in overlappingDeps, for each check whether there's
    -- a conflicting version in userDepsMap, and if so report a conflict
    conflicting :: [DependencyConflictError]
    conflicting = Maybe.mapMaybe createConflict (Map.assocs overlappingDeps)
      where
        createConflict :: (String, String) -> Maybe DependencyConflictError
        createConflict (waspName, waspVersion) =
          Map.lookup waspName userDepsMap >>= c
          where
            c :: String -> Maybe DependencyConflictError
            c userVersion =
              if waspVersion /= userVersion
                then
                  Just $
                    DependencyConflictError
                      (D.create (waspName, waspVersion))
                      (D.create (waspName, userVersion))
                else Nothing

-- | Takes wasp npm dependencies and user npm dependencies and figures out how to
--   combine them together, returning (Right) a list of npm dependencies to be used on
--   behalf of wasp and then also a list of npm dependencies to be used on behalf
--   of user. These lists might be the same as the initial ones, but might also
--   be different.
--   On error (Left), returns list of conflicting user deps together with the error message
--   explaining what the error is.
resolveNpmDeps ::
  [D.Dependency] ->
  [D.Dependency] ->
  Either
    [(D.Dependency, NpmDependenciesConflictError)]
    ([D.Dependency], [D.Dependency])
resolveNpmDeps waspDeps userDeps =
  case resolveDependencies waspDeps userDeps of
    Left conflicts -> Left $ map convertConflict conflicts
    Right (a, b) -> Right (a, b)
  where
    convertConflict :: DependencyConflictError -> (D.Dependency, NpmDependenciesConflictError)
    convertConflict (DependencyConflictError waspDep userDep) =
      ( userDep,
        "Error: Dependency conflict for user dependency ("
          ++ D.name userDep
          ++ ", "
          ++ D.version userDep
          ++ "): "
          ++ "Version must be set to the exactly the same version as"
          ++ " the one wasp is using: "
          ++ D.version waspDep
      )

npmDepsToPackageJsonEntryWithKey :: [D.Dependency] -> String -> String
npmDepsToPackageJsonEntryWithKey deps key =
  "\""
    ++ key
    ++ "\": {"
    ++ intercalate ",\n  " (map (\dep -> "\"" ++ D.name dep ++ "\": \"" ++ D.version dep ++ "\"") deps)
    ++ "\n}"

npmDepsToPackageJsonEntry :: [D.Dependency] -> String
npmDepsToPackageJsonEntry deps = npmDepsToPackageJsonEntryWithKey deps "dependencies"

npmDevDepsToPackageJsonEntry :: [D.Dependency] -> String
npmDevDepsToPackageJsonEntry deps = npmDepsToPackageJsonEntryWithKey deps "devDependencies"
