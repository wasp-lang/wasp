module Wasp.Cli.Command.Version.Download
  ( downloadVersion
  , updateWasp
  , getLatestVersionFromGithub
  , isVersionLessThan
  , forceInstallLatest
  , forceInstallSpecific
  ) where

import Control.Monad (when)
import Control.Exception (try, SomeException)
import Network.HTTP.Simple
  ( httpBS
  , getResponseBody
  , parseRequest
  , setRequestHeader
  , getResponseStatusCode
  , httpNoBody
  , Response
  )
import qualified Data.ByteString.Char8 as BS
import System.IO.Temp (withSystemTempDirectory)
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , copyFile
  , removeDirectoryRecursive
  , renameDirectory
  , removeFile
  , executable
  , setPermissions
  , emptyPermissions
  )
import System.FilePath ((</>))
import System.Process (callProcess, callCommand)
import System.Exit (exitFailure)
import System.Info (os)
import Data.Aeson (decode)
import qualified Data.Aeson.Types as Aeson (parseMaybe, (.:))
import qualified Data.ByteString.Lazy.Char8 as LBS
import Wasp.Cli.Command.Version.Paths 
  ( getVersionPaths
  , getVersionFile
  , getWaspRootDir
  , getWaspBinDir
  )

-- | Update Wasp to latest version
updateWasp :: IO ()
updateWasp = do
  currentVer <- getCurrentReleaseVersion
  latestVer <- getLatestVersionFromGithub
  
  when (isVersionLessThan currentVer latestVer) $ do
    putStrLn $ "Updating from ${currentVer} to ${latestVer}..."
    handleDownloadResult =<< try (downloadVersion latestVer)
    updateSystemMetadata latestVer

getCurrentReleaseVersion :: IO String
getCurrentReleaseVersion = getVersionFile "release" >>= readFile

handleDownloadResult :: Either SomeException () -> IO ()
handleDownloadResult (Left e) = do
  putStrLn $ "Update failed: " ++ show e
  exitFailure
handleDownloadResult (Right _) = return ()

updateSystemMetadata :: String -> IO ()
updateSystemMetadata version = do
  updateMainBinary version
  writeVersionFiles version

writeVersionFiles :: String -> IO ()
writeVersionFiles version = do
  releaseFile <- getVersionFile "release"
  activeFile <- getVersionFile "active"
  writeFile releaseFile version
  writeFile activeFile version

-- | Download and install specific version
downloadVersion :: String -> IO ()
downloadVersion version = do
  -- Step 1: Check if the GitHub release exists
  versionExists <- checkGitHubRelease version
  if not versionExists
    then do
      putStrLn $ "❌ Error: Version " ++ version ++ " does not exist. See https://github.com/wasp-lang/wasp/releases for available versions."
      exitFailure
    else do
      putStrLn $ "Starting download..."

  -- Step 2: Use a temporary directory for download & extraction
  withSystemTempDirectory "wasp-download" $ \tmpDir -> do
    let archiveFile = tmpDir </> getPlatformString
    downloadArchive version archiveFile
    extractArchive archiveFile tmpDir
    _ <- return tmpDir

    versionDir <- getVersionDir version
    ensureCleanInstallation versionDir
    renameDirectory tmpDir versionDir
    putStrLn $ "✅ Wasp version " ++ version ++ " downloaded and activated!"

downloadArchive :: String -> FilePath -> IO ()
downloadArchive version path = do
  let url = "https://github.com/wasp-lang/wasp/releases/download/v" 
         ++ version ++ "/" ++ getPlatformString
  putStrLn $ "Downloading from " ++ url
  request <- parseRequest url
  response <- httpBS request
  BS.writeFile path (getResponseBody response)

extractArchive :: FilePath -> FilePath -> IO ()
extractArchive archivePath destDir = do
  putStrLn "Extracting..."
  callProcess "tar" ["-xzf", archivePath, "-C", destDir]
  removeFile archivePath

ensureCleanInstallation :: FilePath -> IO ()
ensureCleanInstallation path = do
  exists <- doesDirectoryExist path
  when exists $ do
    putStrLn "Removing existing installation..."
    removeDirectoryRecursive path

getVersionDir :: String -> IO FilePath
getVersionDir version = (</> version) <$> getWaspRootDir

-- | Get latest release version from GitHub
getLatestVersionFromGithub :: IO String
getLatestVersionFromGithub = do
  response <- httpBS =<< setRequestHeader "User-Agent" ["wasp-cli"] 
    <$> parseRequest "https://api.github.com/repos/wasp-lang/wasp/releases/latest"
  case decodeResponse (getResponseBody response) of
    Just version -> return $ drop 1 version  -- Remove 'v' prefix
    Nothing -> error "Failed to parse GitHub response"

checkGitHubRelease :: String -> IO Bool
checkGitHubRelease version = do
  let url = "https://github.com/wasp-lang/wasp/releases/download/v" ++ version ++ "/"
  request <- parseRequest url
  result <- try (httpNoBody request) :: IO (Either SomeException (Response()))
  case result of
    Right response ->
      let statusCode = getResponseStatusCode response
      in return (statusCode == 200)
    Left _ -> return False

decodeResponse :: BS.ByteString -> Maybe String
decodeResponse resp = do
  release <- decode (LBS.fromStrict resp)
  Aeson.parseMaybe (Aeson..: "tag_name") release

-- Platform-specific configuration
getPlatformString :: String
getPlatformString = case os of
  "darwin" -> "wasp-macos-x86_64.tar.gz"
  "linux" -> "wasp-linux-x86_64.tar.gz"
  _ -> error $ "Unsupported OS: " ++ os

-- | Create or update the wrapper script
updateWrapperScript :: FilePath -> FilePath -> FilePath -> IO ()
updateWrapperScript name binaryPath dataPath = do
  binDir <- getWaspBinDir
  let wrapperPath = binDir </> name
      wrapperContent = unlines
        [ "#!/usr/bin/env bash"
        , "waspc_datadir=" ++ dataPath ++ " " ++ binaryPath ++ " \"$@\""
        ]
  writeFile wrapperPath wrapperContent
  setPermissions wrapperPath $ emptyPermissions { executable = True }

-- | Update the main wasp binary to a specific version
updateMainBinary :: String -> IO ()
updateMainBinary version = do
  (versionBin, dataDir) <- getVersionPaths version
  binDir <- getWaspBinDir
  let mainBinary = binDir </> "wasp"
  
  exists <- doesFileExist versionBin
  if exists
    then do
      -- Copy the binary
      copyFile versionBin mainBinary
      setPermissions mainBinary $ emptyPermissions { executable = True }
      
      -- Update the wrapper script
      updateWrapperScript "wasp" mainBinary dataDir
      putStrLn "Updated wasp wrapper script"
    else error $ "Version " ++ version ++ " binary not found"

-- | Semantic version comparison
isVersionLessThan :: String -> String -> Bool
isVersionLessThan a b = case (parseVersion a, parseVersion b) of
  (Just v1, Just v2) -> v1 < v2
  _ -> False

parseVersion :: String -> Maybe (Int, Int, Int)
parseVersion v = case reads v of
  [(major, '.':rest1)] -> case reads rest1 of
    [(minor, '.':rest2)] -> case reads rest2 of
      [(patch, "")] -> Just (major, minor, patch)
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing


-- | Force install latest version of Wasp
forceInstallLatest :: IO ()
forceInstallLatest = do
  releaseFile <- getVersionFile "release"
  activeFile <- getVersionFile "active"
  doesFileExist releaseFile >>= flip when (removeFile releaseFile)
  doesFileExist activeFile >>= flip when (removeFile activeFile)
  putStrLn "Forcing installation of the latest version of Wasp..."
  callCommand "curl -sSL https://get.wasp-lang.dev/installer.sh | sh -s"

-- | Force install a specific version of Wasp
forceInstallSpecific :: String -> IO ()
forceInstallSpecific version = do
  releaseFile <- getVersionFile "release"
  activeFile <- getVersionFile "active"
  doesFileExist releaseFile >>= flip when (removeFile releaseFile)
  doesFileExist activeFile >>= flip when (removeFile activeFile)
  putStrLn $ "Forcing installation of Wasp version " ++ version ++ "..."
  callCommand $ "curl -sSL https://get.wasp-lang.dev/installer.sh | sh -s -- -v " ++ version