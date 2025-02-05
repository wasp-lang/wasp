module Wasp.Cli.Command.Version.Executor
  ( executeWithVersion
  , readProcessWithExitCode
  , readProcessWithExitCodeEnv
  ) where

import System.Process (
    proc
  , createProcess
  , waitForProcess
  , StdStream(..)
  , std_in
  , std_out
  , std_err
  , env
  )
import qualified Data.ByteString.Char8 as BS
import Control.Concurrent.MVar
import Control.Monad (unless)
import Control.Exception (evaluate)
import Control.Concurrent (forkIO)
import System.Environment (getEnvironment)
import System.IO (hGetContents, hClose)
import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.Directory (doesFileExist)
import Wasp.Cli.Command.Version.VersionManagement (detectWrapperVersion, getActiveVersion)
import Wasp.Cli.Command.Version.Paths (getVersionPaths, getMainBinaryPath, getVersionFile)


-- | Execute a command using appropriate version
executeWithVersion :: [String] -> IO ()
executeWithVersion args = do
  (activeVer, releaseVer) <- getInstallationVersions

  if activeVer == releaseVer
    then runMainProcess args
    else runVersionedProcess activeVer args
  where
    runMainProcess args' = do
      binPath <- getMainBinaryPath
      (exitCode, _, _) <- readProcessWithExitCode binPath args' ""
      exitWith exitCode

    runVersionedProcess ver args' = do
      (verBin, dataDir) <- getVersionPaths ver
      binExists <- doesFileExist verBin
      if binExists
        then do
          let envVars = [("waspc_datadir", dataDir)]
          (exitCode, _, _) <- readProcessWithExitCodeEnv verBin args' envVars
          exitWith exitCode
        else do
          putStrLn $ "Version " ++ ver ++ " not found in: " ++ verBin
          exitFailure

-- | Helper to execute process with additional environment variables
readProcessWithExitCodeEnv :: FilePath -> [String] -> [(String, String)] -> IO (ExitCode, String, String)
readProcessWithExitCodeEnv cmd args envVars = do
  oldEnv <- getEnvironment
  let newEnv = oldEnv ++ envVars
  (_, Just outh, Just errh, ph) <- 
    createProcess (proc cmd args) { std_out = CreatePipe, std_err = CreatePipe, env = Just newEnv }
  out <- hGetContents outh
  err <- hGetContents errh
  exitCode <- waitForProcess ph
  return (exitCode, out, err)

-- Helper to read process output
readProcessWithExitCode :: FilePath -> [String] -> String -> IO (ExitCode, String, String)
readProcessWithExitCode cmd args stdin = do
  (Just inh, Just outh, Just errh, ph) <- 
    createProcess (proc cmd args){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
  unless (null stdin) $ do
    BS.hPutStr inh (BS.pack stdin)
    hClose inh
  out <- hGetContents outh
  err <- hGetContents errh
  outMVar <- newMVar ""
  errMVar <- newMVar ""
  _ <- forkIO $ evaluate (length out) >> putMVar outMVar out
  _ <- forkIO $ evaluate (length err) >> putMVar errMVar err
  exitCode <- waitForProcess ph
  out' <- takeMVar outMVar
  err' <- takeMVar errMVar
  pure (exitCode, out', err')

-- | Get both active and release versions
getInstallationVersions :: IO (String, String)
getInstallationVersions = do
  active <- getActiveVersion
  release <- getReleaseVersion
  pure (active, release)
  where
    getReleaseVersion = do
      releaseFile <- getVersionFile "release"
      exists <- doesFileExist releaseFile
      if exists then readFile releaseFile else detectWrapperVersion