{-# LANGUAGE CPP #-}

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

import Control.Monad (guard)
import Data.Int (Int32)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.IO (hClose, openTempFile, readFile')
import qualified System.Process as P
import Test.Hspec (expectationFailure)
import Text.Read (readMaybe)
import Wasp.Util.IO.Retry (constPause, retry)
#if !defined(mingw32_HOST_OS)
import Control.Exception (try)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Signals (Signal, nullSignal, sigKILL, signalProcess)
#endif

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

-- The tests that check or kill processes by ID are skipped on Windows, so we
-- only implement this on *nix.
isProcessAlive :: ProcessId -> IO Bool
killProcess :: ProcessId -> IO ()
#if defined(mingw32_HOST_OS)
isProcessAlive _ = fail "isProcessAlive is not supported on Windows"
killProcess _ = fail "killProcess is not supported on Windows"
#else
isProcessAlive pid =
  try (signal nullSignal pid) >>= \case
    Right () -> return True
    Left e
      | isDoesNotExistError e -> return False
      | otherwise -> ioError e

killProcess pid =
  try (signal sigKILL pid) >>= \case
    Right () -> return ()
    Left e
      | isDoesNotExistError e -> return ()
      | otherwise -> ioError e

signal :: Signal -> ProcessId -> IO ()
signal sig (ProcessId pid) = signalProcess sig $ fromIntegral pid
#endif

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
waitUntil label condition =
  retry (constPause 100000) 49 (boolToEither <$> condition) >>= \case
    Right () -> return ()
    Left () -> expectationFailure $ "Timed out waiting for " <> label
  where
    boolToEither True = Right ()
    boolToEither False = Left ()
