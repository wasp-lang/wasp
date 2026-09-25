module Wasp.Job
  ( Job,
    JobOutputSink,
    runJob,
    emitJobOutput,
    failWithExitCode,
    requireExitSuccess,
    getJobOutputSink,
    withBackgroundOutputWorker,
  )
where

import Control.Concurrent (Chan, writeChan)
import qualified Control.Concurrent.Async as Async
import qualified Control.Monad.Catch as Catch
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Text (Text)
import System.Exit (ExitCode (..))
import Wasp.Job.Kind (JobKind)
import Wasp.Job.Output.Event (JobEvent (..), JobEventData (..), JobOutputKind)

type Job = ReaderT JobOutputSink (ExceptT JobFailure (ResourceT IO))

newtype JobFailure = JobFailure Int

type JobOutputSink = JobOutputKind -> Text -> IO ()

runJob :: JobKind -> Job () -> Chan JobEvent -> IO ExitCode
runJob jobKind action chan = do
  result <-
    runResourceT $
      runExceptT $
        runReaderT action outputSink
  let exitCode = either jobFailureExitCode (const ExitSuccess) result
  emitEvent $ JobExited exitCode
  return exitCode
  where
    outputSink outputKind output = emitEvent $ JobOutput outputKind output
    emitEvent eventData =
      writeChan chan $
        JobEvent
          { _eventData = eventData,
            _jobKind = jobKind
          }

jobFailureExitCode :: JobFailure -> ExitCode
jobFailureExitCode (JobFailure exitCode) = ExitFailure exitCode

emitJobOutput :: JobOutputKind -> Text -> Job ()
emitJobOutput outputKind output = do
  emit <- getJobOutputSink
  liftIO $ emit outputKind output

requireExitSuccess :: ExitCode -> Job ()
requireExitSuccess ExitSuccess = return ()
requireExitSuccess (ExitFailure exitCode) = failWithExitCode exitCode

failWithExitCode :: Int -> Job a
failWithExitCode = throwError . JobFailure

getJobOutputSink :: Job JobOutputSink
getJobOutputSink = ask

-- | Stops the worker before returning, including on job failure or cancellation.
withBackgroundOutputWorker :: ((JobOutputKind -> Text -> IO ()) -> IO ()) -> Job a -> Job a
withBackgroundOutputWorker worker action = do
  emit <- getJobOutputSink
  Catch.bracket
    (liftIO $ Async.async $ worker emit)
    (liftIO . Async.cancel)
    (const action)
