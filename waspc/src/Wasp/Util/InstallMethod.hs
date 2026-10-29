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

legacyVersionsNote :: String
legacyVersionsNote = "Wasp versions before 0.27 are published under \"@wasp.sh/wasp-cli\" instead."

uninstallationCommand :: String
uninstallationCommand =
  "npm uninstall -g " ++ npmPackageName

npmPackageName :: String
npmPackageName = "@wasp/cli"
