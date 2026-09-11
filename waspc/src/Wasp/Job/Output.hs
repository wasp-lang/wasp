module Wasp.Job.Output
  ( runAndPrintPrefixedOutput,
    runAndPrintOutput,
    runAndCaptureOutput,
    printEventsPrefixedUntilExit,
    printEvent,
    printEventsUntilExit,
    collectTextUntilExit,
  )
where

import Control.Concurrent (Chan, newChan, readChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import System.Exit (ExitCode)
import System.IO (hFlush)
import qualified Wasp.Job as Job
import Wasp.Job.Output.Internal (getEventContent, getEventOutHandle)
import Wasp.Job.Output.Prefixed (printEventPrefixed, runPrefixedWriter)

runAndPrintPrefixedOutput :: Job.Job -> IO ExitCode
runAndPrintPrefixedOutput job = fst <$> runWithOutput printEventsPrefixedUntilExit job

runAndPrintOutput :: Job.Job -> IO ExitCode
runAndPrintOutput job = fst <$> runWithOutput printEventsUntilExit job

runAndCaptureOutput :: Job.Job -> IO (ExitCode, Text)
runAndCaptureOutput job = do
  (exitCode, chunks) <- runWithOutput collectTextUntilExit job
  return (exitCode, T.concat $ reverse chunks)

runWithOutput :: (Chan Job.JobEvent -> IO a) -> Job.Job -> IO (ExitCode, a)
runWithOutput consumeOutput job = do
  events <- newChan
  Job.runJob job events `concurrently` consumeOutput events

printEventsUntilExit :: Chan Job.JobEvent -> IO ()
printEventsUntilExit = consumeEventsUntilExit $ liftIO . printEvent

printEventsPrefixedUntilExit :: Chan Job.JobEvent -> IO ()
printEventsPrefixedUntilExit chan =
  runPrefixedWriter $ consumeEventsUntilExit printEventPrefixed chan

consumeEventsUntilExit :: (MonadIO m) => (Job.JobEvent -> m ()) -> Chan Job.JobEvent -> m ()
consumeEventsUntilExit consumeEvent chan = do
  event <- liftIO $ readChan chan
  case Job._eventData event of
    Job.JobOutput {} -> consumeEvent event >> consumeEventsUntilExit consumeEvent chan
    Job.JobExited {} -> return ()

collectTextUntilExit :: Chan Job.JobEvent -> IO [Text]
collectTextUntilExit = go []
  where
    go textOutput chan = do
      event <- readChan chan
      case Job._eventData event of
        Job.JobExited {} -> return textOutput
        Job.JobOutput _ text -> go (text : textOutput) chan

printEvent :: Job.JobEvent -> IO ()
printEvent event = do
  let outHandle = getEventOutHandle event
  let message = getEventContent event
  T.IO.hPutStr outHandle message
  hFlush outHandle
