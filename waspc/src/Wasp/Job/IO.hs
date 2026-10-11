module Wasp.Job.IO
  ( JobMessagesConsumer,
    readJobMessagesAndPrintThemPrefixed,
    printJobMessage,
    printJobMsgsUntilExitReceived,
    collectJobTextOutputUntilExitReceived,
  )
where

import Control.Concurrent (Chan, readChan)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text.IO as T.IO
import System.IO (hFlush)
import qualified Wasp.Job as J
import Wasp.Job.Common (getJobMessageContent, getJobMessageOutHandle)
import Wasp.Job.IO.PrefixedWriter (printJobMessagePrefixed, runPrefixedWriter)

-- | Consumes the messages a job writes to its channel, until the job's exit
-- message arrives.
--
-- Wasp runs plenty of processes (npm, tsc, prisma, ...) and their output has to
-- go somewhere. Normally that is the terminal, but `wasp start`'s TUI owns the
-- terminal while it runs and routes the output into its own panes instead. So
-- whatever runs a job takes one of these, leaving the choice to the caller.
type JobMessagesConsumer = Chan J.JobMessage -> IO ()

printJobMsgsUntilExitReceived :: Chan J.JobMessage -> IO ()
printJobMsgsUntilExitReceived chan = do
  jobMsg <- readChan chan
  case J._data jobMsg of
    J.JobOutput {} -> printJobMessage jobMsg >> printJobMsgsUntilExitReceived chan
    J.JobExit {} -> return ()

readJobMessagesAndPrintThemPrefixed :: Chan J.JobMessage -> IO ()
readJobMessagesAndPrintThemPrefixed chan = runPrefixedWriter go
  where
    go = do
      jobMsg <- liftIO $ readChan chan
      case J._data jobMsg of
        J.JobOutput {} -> printJobMessagePrefixed jobMsg >> go
        J.JobExit {} -> return ()

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
