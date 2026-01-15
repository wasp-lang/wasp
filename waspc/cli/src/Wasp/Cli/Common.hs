module Wasp.Cli.Common
  ( CliTemplatesDir,
    waspSays,
    waspWarns,
    waspScreams,
    CliPackagingMode (..),
    getPackagingMode,
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

data CliPackagingMode
  = Installer
  | NpmPackage

-- Keep the environment variable name and value in sync with
-- scripts/make-npm-packages/templates/main-package/bin.js

packagingModeEnvVar :: String
packagingModeEnvVar = "WASP_CLI_PACKAGE_MODE"

getPackagingMode :: IO CliPackagingMode
getPackagingMode =
  lookupEnv packagingModeEnvVar >>= \case
    Just "npm" -> return NpmPackage
    _ -> return Installer
