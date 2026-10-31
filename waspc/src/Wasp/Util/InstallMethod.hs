module Wasp.Util.InstallMethod
  ( getInstallationCommand,
    getProjectInstallationCommand,
    legacyVersionsNote,
    uninstallationCommand,
  )
where

import Data.Maybe (fromMaybe)

-- | Installs the Wasp CLI globally, for use outside of projects (e.g. `wasp new`).
getInstallationCommand :: Maybe String -> String
getInstallationCommand version =
  "npm i -g " ++ npmPackageName ++ "@" ++ versionString
  where
    versionString = fromMaybe "latest" version

-- | Sets the Wasp version a project uses, by updating its @wasp/wasp dependency.
getProjectInstallationCommand :: Maybe String -> String
getProjectInstallationCommand version =
  "npm install --save-exact " ++ projectNpmPackageName ++ "@" ++ versionString
  where
    versionString = fromMaybe "latest" version

legacyVersionsNote :: String
legacyVersionsNote = "Wasp versions before 0.27 are published under \"@wasp.sh/wasp-cli\" instead."

uninstallationCommand :: String
uninstallationCommand =
  "npm uninstall -g " ++ npmPackageName

npmPackageName :: String
npmPackageName = "@wasp/cli"

projectNpmPackageName :: String
projectNpmPackageName = "@wasp/wasp"
