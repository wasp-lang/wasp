module Wasp.Util.InstallMethod
  ( getInstallationCommand,
    legacyVersionsNote,
    uninstallationCommand,
  )
where

import Data.Maybe (fromMaybe)

getInstallationCommand :: Maybe String -> String
getInstallationCommand version =
  "npm i -g " ++ npmPackageName ++ "@" ++ versionString
  where
    versionString = fromMaybe "latest" version

-- | Wasp versions before 0.27 were published under the old `@wasp.sh` npm
-- scope, and we didn't republish them under `@wasp`.
legacyVersionsNote :: String
legacyVersionsNote = "Wasp versions before 0.27 are published as @wasp.sh/wasp-cli instead."

uninstallationCommand :: String
uninstallationCommand =
  "npm uninstall -g " ++ npmPackageName

npmPackageName :: String
npmPackageName = "@wasp/cli"
