module Wasp.Generator.PackageJsonGenerator
  ( resolveNpmDeps,
    resolveDependencies,
    DependencyConflictError (DependencyConflictError),
    npmDepsToPackageJsonEntry,
    npmDevDepsToPackageJsonEntry,
  )
where

import Control.Arrow (ArrowChoice (left))
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Wasp.AppSpec.App.Dependency as D

type NpmDependenciesConflictError = String

-- First entry is Wasp dependency, the second is the user Dependency that is
-- in conflict with it.
data DependencyConflictError = DependencyConflictError D.Dependency D.Dependency
  deriving (Show, Eq)

-- | Takes wasp npm dependencies and user npm dependencies and figures out how
--   to combine them together, returning (Right) a list of npm dependencies to
--   be used on behalf of wasp and then also a list of npm dependencies to be
--   used on behalf of user. These lists might be the same as the initial ones,
--   but might also be different.

--   On error (Left), returns a list of conflicting wasp dep, user dep pairs.
resolveDependencies ::
  [D.Dependency] ->
  [D.Dependency] ->
  Either
    [DependencyConflictError]
    ([D.Dependency], [D.Dependency])
resolveDependencies waspDeps userDeps =
  if null conflictErrors
    then Right (waspDeps, Map.elems userDepsNotInWaspDeps)
    else Left conflictErrors
  where
    waspDepsByName = makeDepsByName waspDeps
    userDepsByName = makeDepsByName userDeps
    overlappingDeps = waspDepsByName `Map.intersection` userDepsByName
    userDepsNotInWaspDeps = userDepsByName `Map.difference` waspDepsByName
    makeDepsByName :: [D.Dependency] -> Map.Map String D.Dependency
    makeDepsByName = Map.fromList . fmap (\d -> (D.name d, d))

    -- get all items in overlappingDeps, for each check whether there's
    -- a conflicting version in userDepsMap, and if so report a conflict
    conflictErrors :: [DependencyConflictError]
    conflictErrors = Maybe.mapMaybe makeConflictErrorIfMismatchedVersion (Map.toAscList overlappingDeps)
      where
        makeConflictErrorIfMismatchedVersion :: (String, D.Dependency) -> Maybe DependencyConflictError
        makeConflictErrorIfMismatchedVersion (waspDepName, waspDep) = do
          userDep <- Map.lookup waspDepName userDepsByName
          if D.version waspDep /= D.version userDep
            then
              Just $
                DependencyConflictError
                  waspDep
                  userDep
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
resolveNpmDeps waspDeps userDeps = left (map convertConflict) $ resolveDependencies waspDeps userDeps
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
