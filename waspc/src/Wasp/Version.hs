module Wasp.Version
  ( waspVersion,
    getWaspCliNpmPackageVersion,
    isRunByWaspCliNpmPackage,
  )
where

import Data.Maybe (fromMaybe, isJust)
import qualified Data.Version as DV
import qualified Paths_waspc
import System.Environment (lookupEnv)
import qualified Wasp.SemanticVersion as SV

-- | The version this binary was built with. Prefer 'getWaspCliNpmPackageVersion'
-- for the version to show or compare, since this one can't be a prerelease.
waspVersion :: SV.Version
waspVersion = case Paths_waspc.version of
  DV.Version [major, minor, patch] _ -> SV.releaseVersion (toEnum major) (toEnum minor) (toEnum patch)
  _ -> error "This should never happen. Wasp binary version must have exactly three digits, but it doesn't."

-- | Falls back to the binary's own version when \@wasp/cli doesn't run it.
getWaspCliNpmPackageVersion :: IO SV.Version
getWaspCliNpmPackageVersion = fromMaybe waspVersion <$> lookupWaspCliNpmPackageVersion

-- | A binary run directly, e.g. one built from source, can't hand over to the
-- project's Wasp, so errors advise it differently.
isRunByWaspCliNpmPackage :: IO Bool
isRunByWaspCliNpmPackage = isJust <$> lookupWaspCliNpmPackageVersion

-- | The env var that \@wasp/cli sets its version to. It tells this binary its
-- version this way, because Cabal versions can't express npm prereleases like
-- `0.27.0-rc.1`.
waspCliNpmPackageVersionEnvVarName :: String
waspCliNpmPackageVersionEnvVarName = "WASP_CLI_NPM_PACKAGE_VERSION"

-- | Fails on a version that doesn't parse, which should never happen.
lookupWaspCliNpmPackageVersion :: IO (Maybe SV.Version)
lookupWaspCliNpmPackageVersion =
  lookupEnv waspCliNpmPackageVersionEnvVarName
    >>= traverse (either (fail . invalidVersionMessage . show) return . SV.parseVersion)
  where
    invalidVersionMessage parseError =
      waspCliNpmPackageVersionEnvVarName ++ " is not a valid version: " ++ parseError
