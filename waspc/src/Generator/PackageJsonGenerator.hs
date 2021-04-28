module Generator.PackageJsonGenerator
  ( resolveNpmDeps,
    toPackageJsonDependenciesString,
  )
where

import Data.List (find, intercalate)
import Data.Maybe (fromJust, isJust)
import qualified NpmDependency as ND

type NpmDependenciesConflictError = String

-- | Takes wasp npm dependencies and user npm dependencies and figures out how to
--   combine them together, returning (Right) a list of npm dependencies to be used on
--   behalf of wasp and then also a list of npm dependencies to be used on behalf
--   of user. These lists might be the same as the initial ones, but might also
--   be different.
--   On error (Left), returns list of conflicting user deps together with the error message
--   explaining what the error is.
resolveNpmDeps ::
  [ND.NpmDependency] ->
  [ND.NpmDependency] ->
  Either
    [(ND.NpmDependency, NpmDependenciesConflictError)]
    ([ND.NpmDependency], [ND.NpmDependency])
resolveNpmDeps waspDeps userDeps =
  if null conflictingUserDeps
    then Right (waspDeps, userDepsNotInWaspDeps)
    else Left conflictingUserDeps
  where
    conflictingUserDeps :: [(ND.NpmDependency, NpmDependenciesConflictError)]
    conflictingUserDeps =
      map (\(dep, err) -> (dep, fromJust err)) $
        filter (isJust . snd) $
          map (\dep -> (dep, checkIfConflictingUserDep dep)) userDeps

    checkIfConflictingUserDep :: ND.NpmDependency -> Maybe NpmDependenciesConflictError
    checkIfConflictingUserDep userDep =
      let attachErrorMessage dep =
            "Error: Dependency conflict for user npm dependency ("
              ++ ND._name dep
              ++ ", "
              ++ ND._version dep
              ++ "): "
              ++ "Version must be set to the exactly the same version as"
              ++ " the one wasp is using: "
              ++ ND._version dep
       in attachErrorMessage <$> find (areTwoDepsInConflict userDep) waspDeps

    areTwoDepsInConflict :: ND.NpmDependency -> ND.NpmDependency -> Bool
    areTwoDepsInConflict d1 d2 =
      ND._name d1 == ND._name d2
        && ND._version d1 /= ND._version d2

    userDepsNotInWaspDeps :: [ND.NpmDependency]
    userDepsNotInWaspDeps = filter (not . isDepWithNameInWaspDeps . ND._name) userDeps

    isDepWithNameInWaspDeps :: String -> Bool
    isDepWithNameInWaspDeps name = any ((name ==) . ND._name) waspDeps

toPackageJsonDependenciesString :: [ND.NpmDependency] -> String
toPackageJsonDependenciesString deps =
  "\"dependencies\": {"
    ++ intercalate ",\n  " (map (\dep -> "\"" ++ ND._name dep ++ "\": \"" ++ ND._version dep ++ "\"") deps)
    ++ "\n}"
