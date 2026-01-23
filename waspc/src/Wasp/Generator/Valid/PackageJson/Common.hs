module Wasp.Generator.Valid.PackageJson.Common where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.Npm.Dependency as D
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import qualified Wasp.Generator.DepVersions as DepVersions
import qualified Wasp.Generator.NpmDependencies as N
import qualified Wasp.Generator.SdkGenerator as Sdk
import qualified Wasp.Generator.ServerGenerator as Server
import qualified Wasp.Generator.WebAppGenerator as WebApp

type DependencySpecification = (P.PackageName, P.PackageVersion)

requiredUserRuntimeDeps :: [DependencySpecification]
requiredUserRuntimeDeps =
  [ -- Installing the wrong version of "react-router-dom" can make users believe that they
    -- can use features that are not available in the version that Wasp supports.
    ("react-router-dom", show DepVersions.reactRouterVersion),
    ("react", show DepVersions.reactVersion),
    ("react-dom", show DepVersions.reactDomVersion)
  ]

requiredUserDevelopmentDeps :: [DependencySpecification]
requiredUserDevelopmentDeps =
  [ ("vite", show DepVersions.viteVersion),
    ("prisma", show DepVersions.prismaVersion)
  ]

optionalUserDeps :: [DependencySpecification]
optionalUserDeps =
  [ ("typescript", show DepVersions.typescriptVersion),
    ("@types/react", show DepVersions.reactTypesVersion),
    ("@types/react-dom", show DepVersions.reactDomTypesVersion),
    ("@types/express", show DepVersions.expressTypesVersion)
  ]

forbiddenUserDeps :: [P.PackageName]
forbiddenUserDeps =
  [ -- The `wasp` package is used in the `workspaces` field of the user's package.json.
    -- It shouldn't be listed as a dependency so it's not overwritten.
    "wasp"
  ]

allCheckedDeps :: AS.AppSpec -> Map P.PackageName P.PackageVersion
allCheckedDeps spec =
  M.fromListWithKey dedupeVersions $
    requiredUserRuntimeDeps
      <> requiredUserDevelopmentDeps
      <> optionalUserDeps
      <> sdkDeps
      <> webAppDeps
      <> serverDeps
  where
    sdkDeps = getAllDepsFromPackage $ Sdk.npmDepsForSdk spec
    webAppDeps = getAllDepsFromPackage $ N.fromWasp $ WebApp.npmDepsFromWasp spec
    serverDeps = getAllDepsFromPackage $ N.fromWasp $ Server.npmDepsFromWasp spec

    dedupeVersions pkgName v1 v2
      | v1 == v2 = v1
      | otherwise =
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
