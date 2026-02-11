module Wasp.Generator.Valid.PackageJson.Common where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.Npm.Dependency as D
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import qualified Wasp.Generator.DepVersions as DepVersions
import qualified Wasp.Generator.NpmDependencies as N
import qualified Wasp.Generator.ServerGenerator as Server
import qualified Wasp.Generator.WebAppGenerator as WebApp

type DependencySpecification = (P.PackageName, P.PackageVersion)

-- | List of dependencies that are required to be present in the user's
-- package.json#dependencies.
requiredUserRuntimeDeps :: [DependencySpecification]
requiredUserRuntimeDeps =
  [ -- Installing the wrong version of "react-router" can make users believe that they
    -- can use features that are not available in the version that Wasp supports.
    ("react-router", show DepVersions.reactRouterVersion),
    ("react", show DepVersions.reactVersion),
    ("react-dom", show DepVersions.reactDomVersion)
  ]

-- | List of dependencies that are required to be present in the user's
-- package.json#devDependencies.
requiredUserDevelopmentDeps :: [DependencySpecification]
requiredUserDevelopmentDeps =
  [ ("vite", show DepVersions.viteVersion),
    ("prisma", show DepVersions.prismaVersion)
  ]

-- | List of dependencies that the user can optionally include in either
-- package.json#dependencies or package.json#devDependencies.
optionalUserDeps :: [DependencySpecification]
optionalUserDeps =
  [ ("typescript", show DepVersions.typescriptVersion),
    ("@types/react", show DepVersions.reactTypesVersion),
    ("@types/react-dom", show DepVersions.reactDomTypesVersion),
    ("@types/express", show DepVersions.expressTypesVersion)
  ]

-- | List of dependencies that the user should NOT include in their package.json.
forbiddenUserDeps :: [P.PackageName]
forbiddenUserDeps =
  [ -- The `wasp` package is used in the `workspaces` field of the user's package.json.
    -- It shouldn't be listed as a dependency so it's not overwritten.
    "wasp"
  ]

-- | Given an AppSpec, returns a Map of all dependencies used across the
-- top-level projects (user, webapp, and server -- not the SDK since that can
-- use its own dependencies). This ensures no package version conflicts occur
-- between different parts of the generated code.
getAllWaspDependencies :: AS.AppSpec -> Map P.PackageName P.PackageVersion
getAllWaspDependencies spec =
  M.fromListWithKey dedupeVersions $
    requiredUserRuntimeDeps
      <> requiredUserDevelopmentDeps
      <> optionalUserDeps
      <> webAppDeps
      <> serverDeps
  where
    webAppDeps = getAllDepsFromPackage $ WebApp.npmDepsFromWasp spec
    serverDeps = getAllDepsFromPackage $ N.fromWasp $ Server.npmDepsFromWasp spec

    dedupeVersions pkgName v1 v2
      | v1 == v2 = v1
      | otherwise =
          -- This should never happen, as we never require two different
          -- versions of the same package. If it does, it's an internal error.
          error $
            "Internal error: Found two different versions for required package "
              ++ show pkgName
              ++ ": "
              ++ show v1
              ++ " /= "
              ++ show v2
              ++ ". This should never happen."

    getAllDepsFromPackage :: N.NpmDepsForPackage -> [DependencySpecification]
    getAllDepsFromPackage pkg =
      concatMap
        (\getDeps -> D.toPair <$> getDeps pkg)
        [ N.dependencies,
          N.devDependencies,
          N.peerDependencies
        ]
