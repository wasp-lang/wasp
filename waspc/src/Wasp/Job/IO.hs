module Wasp.Job.IO
  ( readJobMessagesAndPrintThemPrefixed,
    printJobMessage,
    printJobMsgsUntilExitReceived,
    collectJobTextOutputUntilExitReceived,
  )
where

import Control.Concurrent (Chan, readChan)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text.IO as T.IO
import System.IO (hFlush, hPutStrLn, stderr)
import qualified Wasp.Job as J
import Wasp.Job.Common (getJobMessageContent, getJobMessageOutHandle)
import Wasp.Job.IO.PrefixedWriter (printJobMessagePrefixed, runPrefixedWriter)

printJobMsgsUntilExitReceived :: Chan J.JobMessage -> IO ()
printJobMsgsUntilExitReceived chan = do
  jobMsg <- readChan chan
  case J._data jobMsg of
    J.JobOutput {} -> printJobMessage jobMsg >> printJobMsgsUntilExitReceived chan
    J.JobExit {} -> return ()

readJobMessagesAndPrintThemPrefixed :: Chan J.JobMessage -> IO ()
readJobMessagesAndPrintThemPrefixed chan = runPrefixedWriter go
  where
    debugLog msg = liftIO $ hPutStrLn stderr ("[DEBUG Job.IO] " ++ msg) >> hFlush stderr
    go = do
      debugLog "Waiting for next job message..."
      jobMsg <- liftIO $ readChan chan
      case J._data jobMsg of
        J.JobOutput {} -> do
          debugLog "Received JobOutput, printing and continuing..."
          printJobMessagePrefixed jobMsg >> go
        J.JobExit exitCode -> do
          debugLog $ "Received JobExit: " ++ show exitCode
          return ()

collectJobTextOutputUntilExitReceived :: Chan J.JobMessage -> IO [Text]
collectJobTextOutputUntilExitReceived = go []
  where
    go jobTextOutput chan = do
      jobMsg <- readChan chan
      case J._data jobMsg of
        J.JobExit {} -> return jobTextOutput
        J.JobOutput text _ -> go (text : jobTextOutput) chan

printJobMessage :: J.JobMessage -> IO ()
printJobMessage jobMsg = do
  let outHandle = getJobMessageOutHandle jobMsg
  let message = getJobMessageContent jobMsg
  T.IO.hPutStr outHandle message
  hFlush outHandle
