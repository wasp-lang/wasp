module Test.Process.Util
  ( isPortAvailable,
    isProcessAlive,
    killProcess,
    makeTempPath,
    trim,
    waitUntil,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hClose, openTempFile)
import qualified System.Process as P
import Test.Hspec (expectationFailure)

makeTempPath :: String -> IO FilePath
makeTempPath nameTemplate = do
  tempDir <- getTemporaryDirectory
  (filePath, fileHandle) <- openTempFile tempDir nameTemplate
  hClose fileHandle
  removeFile filePath
  return filePath

isProcessAlive :: String -> IO Bool
isProcessAlive pid = do
  (exitCode, _, _) <-
    P.readCreateProcessWithExitCode
      (P.proc "node" ["-e", "try { process.kill(Number(process.argv[1]), 0); } catch { process.exit(1); }", trim pid])
      ""
  return $ exitCode == ExitSuccess

killProcess :: String -> IO ()
killProcess pid =
  void $
    P.readCreateProcessWithExitCode
      (P.proc "node" ["-e", "try { process.kill(Number(process.argv[1]), 'SIGKILL'); } catch {}", trim pid])
      ""

isPortAvailable :: String -> IO Bool
isPortAvailable port = do
  (exitCode, _, _) <-
    P.readCreateProcessWithExitCode
      (P.proc "node" ["-e", portProbeScript, trim port])
      ""
  return $ exitCode == ExitSuccess
  where
    portProbeScript =
      unlines
        [ "const net = require('node:net');",
          "const server = net.createServer();",
          "server.once('error', () => process.exit(1));",
          "server.listen(Number(process.argv[1]), '127.0.0.1', () => server.close(() => process.exit(0)));"
        ]

trim :: String -> String
trim = unwords . words

waitUntil :: String -> IO Bool -> IO ()
waitUntil label condition = go (50 :: Int)
  where
    go remainingAttempts
      | remainingAttempts <= 0 = expectationFailure $ "Timed out waiting for " <> label
      | otherwise = do
          result <- condition
          if result
            then return ()
            else do
              threadDelay 100000
              go $ remainingAttempts - 1
