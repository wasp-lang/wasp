module Wasp.Cli.Command.Version.Paths
  ( getWaspRootDir
  , getWaspBinDir
  , getMainBinaryPath
  , getVersionFile
  , getVersionPaths
  ) where

import qualified Wasp.Cli.FileSystem as FS
import qualified StrongPath as SP
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.Directory (doesFileExist)

-- Directory structure constants
waspRootDirName :: FilePath
waspRootDirName = ".local/share/wasp-lang"

waspBinDirName :: FilePath
waspBinDirName = ".local/bin"

-- | Get the root directory for Wasp installations
getWaspRootDir :: IO FilePath
getWaspRootDir = do
  homeDir <- SP.fromAbsDir <$> FS.getHomeDir
  return $ homeDir </> waspRootDirName

-- | Get the directory for Wasp binaries
getWaspBinDir :: IO FilePath
getWaspBinDir = do
  homeDir <- SP.fromAbsDir <$> FS.getHomeDir
  return $ homeDir </> waspBinDirName

-- | Path to main wasp wrapper script
getMainBinaryPath :: IO FilePath
getMainBinaryPath = (</> "wasp") <$> getWaspBinDir

-- | Get path to version metadata files
getVersionFile :: String -> IO FilePath
getVersionFile fileName = do
  rootDir <- getWaspRootDir
  return $ rootDir </> fileName

-- | Get paths for a specific version installation
getVersionPaths :: String -> IO (FilePath, FilePath)
getVersionPaths version = do
  rootDir <- getWaspRootDir
  let versionDir = rootDir </> version
  return (versionDir </> "wasp-bin", versionDir </> "data")
