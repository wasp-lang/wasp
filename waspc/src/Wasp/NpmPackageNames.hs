-- | The names of the npm packages Wasp publishes. A Wasp project depends on
-- \@wasp/wasp, which brings the others at its own version.
module Wasp.NpmPackageNames
  ( waspNpmScope,
    waspNpmPackageName,
    waspCliNpmPackageName,
  )
where

-- | The prefix of every package name in the npm scope Wasp publishes to.
waspNpmScope :: String
waspNpmScope = "@wasp/"

waspNpmPackageName :: String
waspNpmPackageName = waspNpmScope ++ "wasp"

waspCliNpmPackageName :: String
waspCliNpmPackageName = waspNpmScope ++ "cli"
