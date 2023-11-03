module Wasp.Generator.Job.IO
  ( readJobMessagesAndPrintThemPrefixed,
    printJobMessage,
    printJobMsgsUntilExitReceived,
    collectJobTextOutputUntilExitReceived,
  )
where

import Control.Concurrent (Chan, readChan)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as T.IO
import System.IO (hFlush)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.Common (getJobMessageContent, getJobMessageOutHandle)
import Wasp.Generator.Job.IO.PrefixedWriter (printJobMessagePrefixed, runPrefixedWriter)

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
collectJobTextOutputUntilExitReceived chan = collectJobMessages chan <&> mapMaybe jobMessageToText
  where
    jobMessageToText :: J.JobMessageData -> Maybe Text
    jobMessageToText = \case
      J.JobOutput text _ -> Just text
      J.JobExit _ -> Nothing

printJobMessage :: J.JobMessage -> IO ()
printJobMessage jobMsg = do
  let outHandle = getJobMessageOutHandle jobMsg
  let message = getJobMessageContent jobMsg
  T.IO.hPutStr outHandle message
  hFlush outHandle

collectJobMessages :: Chan J.JobMessage -> IO [J.JobMessageData]
collectJobMessages = collect []
  where
    collect messages chan = do
      jobMsg <- readChan chan
      case J._data jobMsg of
        J.JobExit {} -> return messages
        message -> collect (message : messages) chan
