module Wasp.Generator.Valid.PackageJson.Common where

import qualified Wasp.ExternalConfig.Npm.PackageJson as P

-- | List of dependencies that the user should NOT include in their package.json.
forbiddenUserDeps :: [P.PackageName]
forbiddenUserDeps =
  [ -- The `wasp` package is used in the `workspaces` field of the user's package.json.
    -- It shouldn't be listed as a dependency so it's not overwritten.
    "wasp"
  ]
