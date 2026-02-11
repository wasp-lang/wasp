module Wasp.Util.InstallMethod
  ( getInstallationCommand,
    uninstallationCommand,
  )
where

import Data.Maybe (fromMaybe)

getInstallationCommand :: Maybe String -> String
getInstallationCommand version =
  "npm i -g " ++ npmPackageName ++ "@" ++ versionString
  where
    versionString = fromMaybe "latest" version

uninstallationCommand :: String
uninstallationCommand =
  "npm uninstall -g " ++ npmPackageName

npmPackageName :: String
npmPackageName = "@wasp.sh/wasp-cli"
