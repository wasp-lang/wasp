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
import qualified Wasp.Job.Kind as Kind
import Wasp.Job.Output.Event (getEventContent, getEventOutHandle)
import qualified Wasp.Job.Output.Event as Event
import Wasp.Job.Output.Prefixed (printEventPrefixed, runPrefixedWriter)

runAndPrintPrefixedOutput :: Kind.JobKind -> Job.Job () -> IO ExitCode
runAndPrintPrefixedOutput jobKind job = withPrefixedOutput $ Job.runJob jobKind job

-- | Prints output until the first JobExited event. The producer must emit
-- an exit event on normal completion so the output consumer can finish.
withPrefixedOutput :: (Chan Event.JobEvent -> IO a) -> IO a
withPrefixedOutput produceEvents =
  fst <$> runWithOutput printEventsPrefixedUntilExit produceEvents

runAndPrintOutput :: Kind.JobKind -> Job.Job () -> IO ExitCode
runAndPrintOutput jobKind job = fst <$> runWithOutput printEventsUntilExit (Job.runJob jobKind job)

runAndCaptureOutput :: Kind.JobKind -> Job.Job () -> IO (ExitCode, Text)
runAndCaptureOutput jobKind job = do
  (exitCode, chunks) <- runWithOutput collectTextUntilExit (Job.runJob jobKind job)
  return (exitCode, T.concat $ reverse chunks)

runWithOutput :: (Chan Event.JobEvent -> IO b) -> (Chan Event.JobEvent -> IO a) -> IO (a, b)
runWithOutput consumeOutput produceEvents = do
  events <- newChan
  produceEvents events `concurrently` consumeOutput events

printEventsUntilExit :: Chan Event.JobEvent -> IO ()
printEventsUntilExit = consumeEventsUntilExit $ liftIO . printEvent

printEventsPrefixedUntilExit :: Chan Event.JobEvent -> IO ()
printEventsPrefixedUntilExit chan =
  runPrefixedWriter $ consumeEventsUntilExit printEventPrefixed chan

consumeEventsUntilExit :: (MonadIO m) => (Event.JobEvent -> m ()) -> Chan Event.JobEvent -> m ()
consumeEventsUntilExit consumeEvent chan = do
  event <- liftIO $ readChan chan
  case Event._eventData event of
    Event.JobOutput {} -> consumeEvent event >> consumeEventsUntilExit consumeEvent chan
    Event.JobExited {} -> return ()

collectTextUntilExit :: Chan Event.JobEvent -> IO [Text]
collectTextUntilExit = go []
  where
    go textOutput chan = do
      event <- readChan chan
      case Event._eventData event of
        Event.JobExited {} -> return textOutput
        Event.JobOutput _ text -> go (text : textOutput) chan

printEvent :: Event.JobEvent -> IO ()
printEvent event = do
  let outHandle = getEventOutHandle event
  let message = getEventContent event
  T.IO.hPutStr outHandle message
  hFlush outHandle
