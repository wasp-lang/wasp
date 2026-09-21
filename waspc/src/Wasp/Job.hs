module Wasp.Job
  ( Job,
    JobAction,
    JobEvent (..),
    JobEventData (..),
    JobOutputKind (..),
    JobKind (..),
    JobOutputSink,
    makeJob,
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

data Job = Job JobKind (JobAction ())

type JobAction = ReaderT JobOutputSink (ExceptT JobFailure (ResourceT IO))

newtype JobFailure = JobFailure Int

type JobOutputSink = JobOutputKind -> Text -> IO ()

data JobEvent = JobEvent
  { _eventData :: JobEventData,
    _jobKind :: JobKind
  }
  deriving (Show)

data JobEventData
  = JobOutput JobOutputKind Text
  | JobExited ExitCode
  deriving (Show)

data JobOutputKind = Stdout | Stderr deriving (Show, Eq)

data JobKind = WebApp | Server | Db | Wasp deriving (Show, Eq, Ord, Bounded, Enum)

makeJob :: JobKind -> JobAction () -> Job
makeJob = Job

runJob :: Job -> Chan JobEvent -> IO ExitCode
runJob (Job jobKind action) chan = do
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

emitJobOutput :: JobOutputKind -> Text -> JobAction ()
emitJobOutput outputKind output = do
  emit <- getJobOutputSink
  liftIO $ emit outputKind output

requireExitSuccess :: ExitCode -> JobAction ()
requireExitSuccess ExitSuccess = return ()
requireExitSuccess (ExitFailure exitCode) = failWithExitCode exitCode

failWithExitCode :: Int -> JobAction a
failWithExitCode = throwError . JobFailure

getJobOutputSink :: JobAction JobOutputSink
getJobOutputSink = ask

-- | Stops the worker before returning, including on job failure or cancellation.
withBackgroundOutputWorker :: ((JobOutputKind -> Text -> IO ()) -> IO ()) -> JobAction a -> JobAction a
withBackgroundOutputWorker worker action = do
  emit <- getJobOutputSink
  Catch.bracket
    (liftIO $ Async.async $ worker emit)
    (liftIO . Async.cancel)
    (const action)
