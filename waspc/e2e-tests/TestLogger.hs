module TestLogger
  ( TestLogger,
    withTestLogger,
    logStepHeader,
    logOutputChunk,
    formatFailureWithLog,
  )
where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (bracket)
import qualified Data.ByteString as BS
import StrongPath (Abs, File, File', Path', castFile, fromAbsFile)
import System.IO (Handle, IOMode (WriteMode), hClose, hFlush, hPutStr, openFile, readFile')

-- | Collects the output of a single test (or snapshot test) into its log file.
-- Writes are guarded by a lock so that concurrent writers (e.g. the stdout and
-- stderr drains of a running command) don't interleave mid-chunk.
data TestLogger = TestLogger
  { logFile :: Path' Abs File',
    logHandle :: Handle,
    logLock :: MVar ()
  }

withTestLogger :: Path' Abs (File f) -> String -> (TestLogger -> IO a) -> IO a
withTestLogger logFile testName action =
  bracket openLog (hClose . (.logHandle)) action
  where
    openLog = do
      logHandle <- openFile (fromAbsFile logFile) WriteMode
      hPutStr logHandle $ "=== " ++ testName ++ " ===\n"
      hFlush logHandle
      logLock <- newMVar ()
      return TestLogger {logFile = castFile logFile, logHandle, logLock}

loggerLogFile :: TestLogger -> FilePath
loggerLogFile = fromAbsFile . (.logFile)

-- | Writes a header marking the start of a step, so each step's output can be
-- told apart in the log.
logStepHeader :: TestLogger -> String -> IO ()
logStepHeader logger stepDescription =
  writeLocked logger $ \handle -> do
    hPutStr handle $ "\n--- step: " ++ stepDescription ++ " ---\n"
    hFlush handle

-- | Writes a raw chunk of command output to the log.
logOutputChunk :: TestLogger -> BS.ByteString -> IO ()
logOutputChunk logger chunk =
  writeLocked logger $ \handle -> do
    BS.hPut handle chunk
    hFlush handle

-- | Formats a failure message so it points at the log and includes its content,
-- since the log is the main tool for debugging a failed test.
-- Closes the log, since GHC's file locking does not allow reading a file this
-- process has open for writing (and a failed test won't log anything further).
formatFailureWithLog :: TestLogger -> String -> IO String
formatFailureWithLog logger failureMessage = do
  writeLocked logger hClose
  logContent <- readFile' (loggerLogFile logger)
  return $
    unlines
      [ failureMessage,
        "See log: " ++ loggerLogFile logger,
        "=== Log content ===",
        logContent
      ]

writeLocked :: TestLogger -> (Handle -> IO ()) -> IO ()
writeLocked logger action = withMVar logger.logLock $ \() -> action logger.logHandle
