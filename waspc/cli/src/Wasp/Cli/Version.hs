module Wasp.Cli.Version
  ( printVersion,
  )
where

import Control.Monad (guard)
import Data.Char (isSpace)
import Data.Functor (($>))
import Data.List (dropWhileEnd, intersperse)
import Data.Maybe (catMaybes, fromMaybe)
import System.IO.Error (catchIOError)
import qualified System.Info
import System.Process (readProcessWithExitCode)
import qualified Wasp.Node.Version as NodeVersion
import Wasp.Util (eitherToMaybe)
import Wasp.Util.GitRev (gitRevDescription)
import Wasp.Util.InstallMethod (installInstructions)
import Wasp.Version (waspVersion)

-- | Prints the Wasp CLI version. With the `--full` flag it also prints
-- diagnostic information useful for bug reports: the git commit the binary was
-- built from, the operating system, and the user's Node.js and npm versions.
printVersion :: [String] -> IO ()
printVersion versionArgs = do
  (putStr . unlines . intersperse "") =<< sequence (catMaybes versionInfoParts)
  where
    versionInfoParts =
      [ Just $ return $ show waspVersion,
        guard ("--full" `elem` versionArgs)
          $> getFullVersionInfo,
        Just $ return installInstructions
      ]

getFullVersionInfo :: IO String
getFullVersionInfo =
  makeFullVersionInfo
    <$> getOsInfo
    <*> (eitherToMaybe <$> NodeVersion.getUserNodeVersion)
    <*> (eitherToMaybe <$> NodeVersion.getUserNpmVersion)
  where
    makeFullVersionInfo osInfo nodeVersion npmVersion =
      unlines
        [ "Git commit: " ++ ifKnown gitRevDescription,
          "OS:         " ++ osInfo,
          "Node:       " ++ ifKnown (show <$> nodeVersion),
          "npm:        " ++ ifKnown (show <$> npmVersion)
        ]

    ifKnown = fromMaybe "unknown"

-- | Returns a string like "darwin 24.5.0 aarch64" combining the operating
-- system name, its release/version, and the architecture.
getOsInfo :: IO String
getOsInfo = do
  osVersion <- getOsVersion
  return $ unwords $ filter (not . null) [System.Info.os, osVersion, System.Info.arch]

-- | Returns the OS release/version via `uname -r`, or an empty string if it
-- can't be determined (e.g. on systems without `uname`).
getOsVersion :: IO String
getOsVersion = getVersionFromUname `catchIOError` const (return "")
  where
    getVersionFromUname = do
      (_exitCode, stdout, _stderr) <- readProcessWithExitCode "uname" ["-r"] ""
      return $ trim stdout
    trim = dropWhileEnd isSpace . dropWhile isSpace
