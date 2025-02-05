module Wasp.Cli.Command.Version.VersionManagement
  ( switchVersion
  , ensureVersionSystem
  , getActiveVersion
  , listVersions
  , isVersionLessThan
  , detectWrapperVersion
  ) where

import Control.Monad (unless, when, filterM)
import qualified Control.Exception as E
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , doesDirectoryExist
  , listDirectory
  , executable
  , setPermissions
  , emptyPermissions
  )
import System.FilePath (takeFileName, takeDirectory, (</>))
import System.Exit (exitFailure)
import qualified Wasp.Version as WV
import Wasp.Cli.Command.Version.Download
  ( getLatestVersionFromGithub
  , downloadVersion
  , isVersionLessThan
  )
import Wasp.Cli.Command.Version.Paths
  ( getVersionPaths
  , getMainBinaryPath
  , getVersionFile
  , getWaspRootDir
  )
-- | Switch to specified version
switchVersion :: String -> IO ()
switchVersion version = do
  result <- E.try (performVersionSwitch version) :: IO (Either E.SomeException ())
  case result of
    Left e -> do
      putStrLn $ "Error: Failed to switch version due to: " ++ show e
      putStrLn "Try running 'wasp version latest --force' to install the latest version."
      exitFailure
    Right _ -> do
      putStrLn $ "🐝 Wasp version " ++ version ++ " is now active!"
      return ()

performVersionSwitch :: String -> IO ()
performVersionSwitch "latest" = handleLatest
performVersionSwitch version = handleSpecificVersion version

handleLatest :: IO ()
handleLatest = getLatestVersionFromGithub >>= switchVersion

handleSpecificVersion :: String -> IO ()
handleSpecificVersion version = do
  (binPath, _) <- getVersionPaths version
  binExists <- doesFileExist binPath
  
  unless binExists $ handleMissingVersion version
  makeExecutable binPath
  updateVersionMetadata version

handleMissingVersion :: String -> IO ()
handleMissingVersion version = do
  putStrLn $ "Downloading Wasp version " ++ version ++ "..."
  result <- E.try (downloadVersion version) :: IO (Either E.SomeException ())
  case result of
    Left e -> do
      putStrLn $ "Download failed for version " ++ version
      putStrLn "Try running 'wasp version latest --force' to install the latest version."
      putStrLn $ "Error details: " ++ show e
      exitFailure
    Right _ -> return ()

makeExecutable :: FilePath -> IO ()
makeExecutable path = do
  exists <- doesFileExist path
  when exists $ do
    let perms = emptyPermissions { executable = True }
    setPermissions path perms

updateVersionMetadata :: String -> IO ()
updateVersionMetadata version = do
  updateReleaseIfNeeded version
  updateActiveVersion version

updateReleaseIfNeeded :: String -> IO ()
updateReleaseIfNeeded version = do
  releaseFile <- getVersionFile "release"
  currentRelease <- safeReadFile releaseFile
  when (maybe False (isVersionLessThan version) currentRelease) $
    writeFile releaseFile version

updateActiveVersion :: String -> IO ()
updateActiveVersion version = do
  activeFile <- getVersionFile "active"
  createParentDirectories
  writeFile activeFile version
  where
    createParentDirectories = getWaspRootDir >>= createDirectoryIfMissing True

-- | List all installed versions
listVersions :: IO [String]
listVersions = do
  rootDir <- getWaspRootDir
  dirs <- listDirectory rootDir
  filterM (isValidVersion rootDir) dirs
  where
    isValidVersion root name = do
      isDir <- doesDirectoryExist (root </> name)
      return $ isDir && name `notElem` [".", "..", "active", "release"]

-- | Initialize version management system
ensureVersionSystem :: IO ()
ensureVersionSystem = do
  rootDir <- getWaspRootDir
  createDirectoryIfMissing True rootDir
  
  releaseFile <- getVersionFile "release"
  ensureFileExists releaseFile detectWrapperVersion
  
  activeFile <- getVersionFile "active"
  ensureFileExists activeFile (getReleaseVersion releaseFile)

  ensureReleaseVersionConsistency

ensureFileExists :: FilePath -> IO String -> IO ()
ensureFileExists path getDefaultContent = do
  exists <- doesFileExist path
  unless exists $ getDefaultContent >>= writeFile path

getReleaseVersion :: FilePath -> IO String
getReleaseVersion releaseFile = do
  exists <- doesFileExist releaseFile
  if exists then readFile releaseFile else detectWrapperVersion

ensureReleaseVersionConsistency :: IO ()
ensureReleaseVersionConsistency = do
  releaseFile <- getVersionFile "release"
  currentVer <- detectWrapperVersion
  releaseExists <- doesFileExist releaseFile

  when releaseExists $ do
    releaseVer <- readFile releaseFile
    unless (releaseVer == currentVer) $ do
      writeFile releaseFile currentVer
      syncActiveVersionIfNeeded releaseVer currentVer

syncActiveVersionIfNeeded :: String -> String -> IO ()
syncActiveVersionIfNeeded oldRelease newRelease = do
  activeVer <- getActiveVersion
  when (activeVer == oldRelease) $ do
    activeFile <- getVersionFile "active"
    writeFile activeFile newRelease

-- | Get currently active version
getActiveVersion :: IO String
getActiveVersion = do
  activeFile <- getVersionFile "active"
  safeReadFile activeFile >>= maybe getFallbackVersion return
  where
    getFallbackVersion = do
      releaseFile <- getVersionFile "release"
      safeReadFile releaseFile >>= maybe (return $ show WV.waspVersion) return

safeReadFile :: FilePath -> IO (Maybe String)
safeReadFile path = do
  exists <- doesFileExist path
  if exists then Just <$> readFile path else return Nothing

-- | Extracts the version from the wrapper script
detectWrapperVersion :: IO String
detectWrapperVersion = do
  binPath <- getMainBinaryPath
  fileExists <- doesFileExist binPath
  if fileExists
    then do
      content <- readFile binPath
      let version = extractVersionFromScript content
      return (fromMaybe "unknown" version)
    else return "unknown"

-- | Extract version from wrapper script content
extractVersionFromScript :: String -> Maybe String
extractVersionFromScript script =
  case filter ("wasp-bin" `isInfixOf`) (lines script) of
    (line:_) -> extractVersionFromPath line
    _        -> Nothing

-- | Extract version number from the wrapper script's binary path using regex.
extractVersionFromPath :: String -> Maybe String
extractVersionFromPath line =
  let parts = words line
      maybePath = case filter ("/wasp-lang/" `isInfixOf`) parts of
                    (p:_) -> Just p
                    _     -> Nothing
  in fmap (takeFileName . takeDirectory) maybePath
