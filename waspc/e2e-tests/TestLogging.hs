module TestLogging (openLogForCommand) where

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
