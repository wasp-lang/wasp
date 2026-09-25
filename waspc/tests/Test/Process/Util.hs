module Test.Process.Util
  ( ProcessId,
    readProcessId,
    isPortAvailable,
    isProcessAlive,
    killProcess,
    makeTempPath,
    trim,
    waitUntil,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (guard)
import Data.Int (Int32)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.IO (hClose, openTempFile, readFile')
import qualified System.Process as P
import Test.Hspec (expectationFailure)
import Text.Read (readMaybe)

newtype ProcessId = ProcessId Int32
  deriving (Eq, Show)

parseProcessId :: String -> Maybe ProcessId
parseProcessId text = do
  value <- readMaybe text :: Maybe Integer
  guard $ value > 0 && value <= fromIntegral (maxBound :: Int32)
  return $ ProcessId $ fromIntegral value

readProcessId :: FilePath -> IO ProcessId
readProcessId path = do
  contents <- readFile' path
  maybe (fail $ "Invalid process ID in " <> path) return $ parseProcessId contents

makeTempPath :: String -> IO FilePath
makeTempPath nameTemplate = do
  tempDir <- getTemporaryDirectory
  (filePath, fileHandle) <- openTempFile tempDir nameTemplate
  hClose fileHandle
  removeFile filePath
  return filePath

isProcessAlive :: ProcessId -> IO Bool
isProcessAlive (ProcessId pid) = do
  (exitCode, _, stderr) <-
    P.readCreateProcessWithExitCode
      (P.proc "node" ["-e", "try { process.kill(Number(process.argv[1]), 0); } catch (error) { if (error.code === 'ESRCH') process.exit(3); throw error; }", show pid])
      ""
  case exitCode of
    ExitSuccess -> return True
    ExitFailure 3 -> return False
    _ -> fail $ "Could not check process " <> show pid <> ": " <> stderr

killProcess :: ProcessId -> IO ()
killProcess (ProcessId pid) =
  P.callProcess "node" ["-e", "try { process.kill(Number(process.argv[1]), 'SIGKILL'); } catch (error) { if (error.code !== 'ESRCH') throw error; }", show pid]

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
