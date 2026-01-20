module Wasp.Util.InstallMethod
  ( CliInstallMethod (..),
    waspCliInstallMethod,
    getInstallationCommand,
    npmPackageName,
  )
where

import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

data CliInstallMethod
  = BinaryInstaller
  | NpmPackage

-- Keep the environment variable name and value in sync with
-- scripts/make-npm-packages/templates/main-package/bin.js
installMethodEnvVarName :: String
installMethodEnvVarName = "WASP_CLI_INSTALL_METHOD"

{-# NOINLINE waspCliInstallMethod #-}
waspCliInstallMethod :: CliInstallMethod
waspCliInstallMethod =
  -- We're using `unsafePerformIO` here to read the environment variable only once,
  -- since we're treating it as a static value it's fine to treat this as a compiled-in
  -- value.
  case unsafePerformIO $ lookupEnv installMethodEnvVarName of
    Just "npm" -> NpmPackage
    _ -> BinaryInstaller

npmPackageName :: String
npmPackageName = "@wasp.sh/wasp-cli"

getInstallationCommand :: CliInstallMethod -> Maybe String -> String
getInstallationCommand NpmPackage (Just version) =
  "npm i -g " ++ npmPackageName ++ "@" ++ version
getInstallationCommand NpmPackage Nothing =
  "npm i -g " ++ npmPackageName ++ "@latest"
getInstallationCommand BinaryInstaller (Just version) =
  "curl -sSL https://get.wasp.sh/installer.sh | sh -s -- -v " ++ version
getInstallationCommand BinaryInstaller Nothing =
  "curl -sSL https://get.wasp.sh/installer.sh | sh -s"
