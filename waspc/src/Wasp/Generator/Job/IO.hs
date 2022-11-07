module Wasp.Generator.Job.IO
  ( readJobMessagesAndPrintThemPrefixed,
    printJobMessage,
  )
where

import Control.Concurrent (Chan, readChan)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.IO as T.IO
import System.IO (hFlush)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.Common (getJobMessageContent, getJobMessageOutHandle)
import Wasp.Generator.Job.IO.PrefixedWriter (printJobMessagePrefixed, runPrefixedWriter)

readJobMessagesAndPrintThemPrefixed :: Chan J.JobMessage -> IO ()
readJobMessagesAndPrintThemPrefixed chan = runPrefixedWriter go
  where
    go = do
      jobMsg <- liftIO $ readChan chan
      case J._data jobMsg of
        J.JobOutput {} -> printJobMessagePrefixed jobMsg >> go
        J.JobExit {} -> return ()

printJobMessage :: J.JobMessage -> IO ()
printJobMessage jobMsg = do
  let outHandle = getJobMessageOutHandle jobMsg
  let message = getJobMessageContent jobMsg
  T.IO.hPutStr outHandle message
  hFlush outHandle
