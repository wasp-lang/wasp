module Wasp.Job.Output
  ( runAndPrintPrefixedOutput,
    withPrefixedOutput,
    runAndPrintOutput,
    runAndCaptureOutput,
    printEventsPrefixedUntilExit,
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
runAndPrintPrefixedOutput job = withPrefixedOutput $ Job.runJob job

-- | Prints output until the first JobExited event. The producer must emit
-- an exit event on normal completion so the output consumer can finish.
withPrefixedOutput :: (Chan Job.JobEvent -> IO a) -> IO a
withPrefixedOutput produceEvents =
  fst <$> runWithOutput printEventsPrefixedUntilExit produceEvents

runAndPrintOutput :: Job.Job -> IO ExitCode
runAndPrintOutput job = fst <$> runWithOutput printEventsUntilExit (Job.runJob job)

runAndCaptureOutput :: Job.Job -> IO (ExitCode, Text)
runAndCaptureOutput job = do
  (exitCode, chunks) <- runWithOutput collectTextUntilExit (Job.runJob job)
  return (exitCode, T.concat $ reverse chunks)

runWithOutput :: (Chan Job.JobEvent -> IO b) -> (Chan Job.JobEvent -> IO a) -> IO (a, b)
runWithOutput consumeOutput produceEvents = do
  events <- newChan
  produceEvents events `concurrently` consumeOutput events

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
