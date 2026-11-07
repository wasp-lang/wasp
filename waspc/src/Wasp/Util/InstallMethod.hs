module Wasp.Util.InstallMethod
  ( getGlobalInstallationCommand,
    getProjectInstallationCommand,
    legacyVersionsNote,
    uninstallationCommand,
  )
where

import Data.Maybe (fromMaybe)
import Wasp.NpmPackageNames (waspCliNpmPackageName, waspNpmPackageName)

getGlobalInstallationCommand :: Maybe String -> String
getGlobalInstallationCommand version =
  "npm i -g " ++ waspCliNpmPackageName ++ "@" ++ versionString
  where
    versionString = fromMaybe "latest" version

-- | Sets a project's Wasp version.
getProjectInstallationCommand :: String -> String
getProjectInstallationCommand version =
  "npm install --save-exact " ++ waspNpmPackageName ++ "@" ++ version

legacyVersionsNote :: String
legacyVersionsNote = "Wasp versions before 0.27 are published under \"@wasp.sh/wasp-cli\" instead."

uninstallationCommand :: String
uninstallationCommand =
  "npm uninstall -g " ++ waspCliNpmPackageName
