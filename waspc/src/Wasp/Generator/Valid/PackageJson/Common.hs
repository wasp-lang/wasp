module Wasp.Generator.Valid.PackageJson.Common where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.Npm.Dependency as D
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import qualified Wasp.Generator.NpmDependencies as N
import qualified Wasp.Generator.ServerGenerator as Server

type DependencySpecification = (P.PackageName, P.PackageVersion)

-- | List of dependencies that the user should NOT include in their package.json.
forbiddenUserDeps :: [P.PackageName]
forbiddenUserDeps =
  [ -- The `wasp` package is used in the `workspaces` field of the user's package.json.
    -- It shouldn't be listed as a dependency so it's not overwritten.
    "wasp"
  ]

-- | The dependencies of the generated server, regular and dev. If the user's
-- package.json lists one of them too, it must use the same version, so npm
-- installs one copy.
--
-- The SDK's dependencies are left out, since the SDK can use its own copies.
-- The core dependencies are left out too: they are peer dependencies of
-- \@wasp/wasp, and npm checks those.
--
-- TODO(#4824): Once the optional dependencies move to the user's project, the
-- generated server lists none, so this check and `wasp.overriddenDeps` go away.
getGeneratedServerDeps :: AS.AppSpec -> Map P.PackageName P.PackageVersion
getGeneratedServerDeps spec =
  M.fromListWithKey dedupeVersions serverDeps
  where
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
          N.devDependencies
        ]
