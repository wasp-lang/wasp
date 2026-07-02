module TestLogging (openLogForCommand, formatCommandFailure) where

import FileSystem (TestLogFile)
import GHC.IO.Handle (hDuplicate)
import StrongPath (Abs, File, Path', fromAbsFile)
import System.IO (Handle, IOMode (WriteMode), hFlush, hPutStr, openFile)

openLogForCommand :: Path' Abs (File TestLogFile) -> String -> String -> IO (Handle, Handle)
openLogForCommand logFile testName command = do
  hOut <- openFile (fromAbsFile logFile) WriteMode
  hPutStr hOut $
    unlines
      [ "=== " ++ testName ++ " ===",
        "=== Command ===",
        command,
        "=== Output ===",
        ""
      ]
  hFlush hOut
  hErr <- hDuplicate hOut
  return (hOut, hErr)

formatCommandFailure :: Int -> Path' Abs (File TestLogFile) -> IO String
formatCommandFailure exitCode logFile = do
  logContent <- readFile (fromAbsFile logFile)
  return $
    unlines
      [ "Command failed with exit code " ++ show exitCode ++ ". See log: " ++ fromAbsFile logFile,
        "=== Log content ===",
        logContent
      ]
