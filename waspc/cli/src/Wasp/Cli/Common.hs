module Wasp.Cli.Common
  ( CliTemplatesDir,
    waspSays,
    waspWarns,
    waspScreams,
    CliInstallMethod (..),
    getInstallMethod,
  )
where

import System.Environment (lookupEnv)
import qualified Wasp.Util.Terminal as Term

data CliTemplatesDir

waspSays :: String -> IO ()
waspSays what = putStrLn $ Term.applyStyles [Term.Yellow] what

waspWarns :: String -> IO ()
waspWarns what = putStrLn $ Term.applyStyles [Term.Magenta] what

waspScreams :: String -> IO ()
waspScreams what = putStrLn $ Term.applyStyles [Term.Red] what

data CliInstallMethod
  = BinaryInstaller
  | NpmPackage

-- Keep the environment variable name and value in sync with
-- scripts/make-npm-packages/templates/main-package/bin.js

installMethodEnvVarName :: String
installMethodEnvVarName = "WASP_CLI_INSTALL_METHOD"

getInstallMethod :: IO CliInstallMethod
getInstallMethod =
  lookupEnv installMethodEnvVarName >>= \case
    Just "npm" -> return NpmPackage
    _ -> return BinaryInstaller
