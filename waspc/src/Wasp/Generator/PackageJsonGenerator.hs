module Wasp.Generator.PackageJsonGenerator
  ( resolveNpmDeps,
    npmDepsToPackageJsonEntry,
    npmDevDepsToPackageJsonEntry,
  )
where

import Data.Bifunctor (second)
import Data.List (find, intercalate)
import Data.Maybe (fromJust, isJust)
import qualified Wasp.AppSpec.App.Dependency as D

type NpmDependenciesConflictError = String

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
  if null conflictingUserDeps
    then Right (waspDeps, userDepsNotInWaspDeps)
    else Left conflictingUserDeps
  where
    conflictingUserDeps :: [(D.Dependency, NpmDependenciesConflictError)]
    conflictingUserDeps =
      map (second fromJust) $
        filter (isJust . snd) $
          map (\dep -> (dep, checkIfConflictingUserDep dep)) userDeps

    checkIfConflictingUserDep :: D.Dependency -> Maybe NpmDependenciesConflictError
    checkIfConflictingUserDep userDep =
      let attachErrorMessage dep =
            "Error: Dependency conflict for user dependency ("
              ++ D.name dep
              ++ ", "
              ++ D.version dep
              ++ "): "
              ++ "Version must be set to the exactly the same version as"
              ++ " the one wasp is using: "
              ++ D.version dep
       in attachErrorMessage <$> find (areTwoDepsInConflict userDep) waspDeps

    areTwoDepsInConflict :: D.Dependency -> D.Dependency -> Bool
    areTwoDepsInConflict d1 d2 =
      D.name d1 == D.name d2
        && D.version d1 /= D.version d2

    userDepsNotInWaspDeps :: [D.Dependency]
    userDepsNotInWaspDeps = filter (not . isDepWithNameInWaspDeps . D.name) userDeps

    isDepWithNameInWaspDeps :: String -> Bool
    isDepWithNameInWaspDeps name = any ((name ==) . D.name) waspDeps

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
