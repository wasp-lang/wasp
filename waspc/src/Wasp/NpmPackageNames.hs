-- | The names of the npm packages Wasp publishes. A Wasp project depends on
-- \@wasp/wasp, which brings the others at its own version.
module Wasp.NpmPackageNames
  ( waspNpmPackageName,
    waspCliNpmPackageName,
    waspSpecNpmPackageName,
  )
where

waspNpmPackageName :: String
waspNpmPackageName = "@wasp/wasp"

waspCliNpmPackageName :: String
waspCliNpmPackageName = "@wasp/cli"

waspSpecNpmPackageName :: String
waspSpecNpmPackageName = "@wasp/spec"
