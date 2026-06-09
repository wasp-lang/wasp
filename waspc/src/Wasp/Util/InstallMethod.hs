module Wasp.Util.InstallMethod
  ( getInstallationCommand,
    uninstallationCommand,
    installInstructions,
  )
where

import Data.Maybe (fromMaybe)
import Wasp.Util (indent)

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

installInstructions :: String
installInstructions =
  unlines
    [ "If you wish to install/switch to the latest version of Wasp, do:",
      indent 2 $ getInstallationCommand Nothing,
      "",
      "If you want a specific x.y.z version of Wasp, do:",
      indent 2 $ getInstallationCommand $ Just "x.y.z",
      "",
      "Check https://github.com/wasp-lang/wasp/releases for the list of valid versions, including the latest one."
    ]
