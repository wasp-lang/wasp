module Wasp.Job
  ( Job,
    JobKind (..),
    Stream (..),
    Sink,
    Printer,
    JobFailure,
    jobFailureExitCode,
    jobFailureMessage,
    runJob,
    withKind,
    describeFailure,
    emitJobOutput,
    failWithExitCode,
    requireExitSuccess,
    getSink,
    withBackgroundOutputWorker,
  )
where

import qualified Control.Concurrent.Async as Async
import qualified Control.Monad.Catch as Catch
import Control.Monad.Except (ExceptT, MonadError (catchError, throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Exit (ExitCode (..))

type Job = ReaderT JobEnv (ExceptT JobFailure (ResourceT IO))

data JobEnv = JobEnv
  { _printer :: Printer,
    _kind :: JobKind
  }

-- | Labels a job's output, e.g. for 'Wasp.Job.Output.withPrefixed'.
data JobKind = WebApp | Server | Db | Wasp deriving (Show, Eq, Ord, Bounded, Enum)

data Stream = Stdout | Stderr deriving (Show, Eq, Ord)

-- | Receives a job's output. Jobs can call it from several threads at once.
type Sink = Stream -> Text -> IO ()

-- | Gives the sink for the output of each kind of job.
type Printer = JobKind -> Sink

data JobFailure = JobFailure
  { _exitCode :: Int,
    _message :: Maybe String
  }

jobFailureExitCode :: JobFailure -> Int
jobFailureExitCode = _exitCode

-- | The message set with 'describeFailure', or a generic one.
jobFailureMessage :: JobFailure -> String
jobFailureMessage failure =
  fromMaybe ("Job failed with exit code " <> show (_exitCode failure) <> ".") $ _message failure

-- | Returns once the job has finished and its resources are released. The
-- output of a job that doesn't set its kind with 'withKind' is labelled as
-- Wasp's own.
runJob :: Printer -> Job () -> IO (Either JobFailure ())
runJob printer job =
  runResourceT $ runExceptT $ runReaderT job $ JobEnv {_printer = printer, _kind = Wasp}

withKind :: JobKind -> Job a -> Job a
withKind kind = local $ \env -> env {_kind = kind}

-- | Sets the message the job fails with, given its exit code.
describeFailure :: (Int -> String) -> Job a -> Job a
describeFailure describe job =
  job `catchError` \failure ->
    throwError failure {_message = Just $ describe $ _exitCode failure}

emitJobOutput :: Stream -> Text -> Job ()
emitJobOutput stream output = do
  sink <- getSink
  liftIO $ sink stream output

requireExitSuccess :: ExitCode -> Job ()
requireExitSuccess ExitSuccess = return ()
requireExitSuccess (ExitFailure exitCode) = failWithExitCode exitCode

failWithExitCode :: Int -> Job a
failWithExitCode exitCode = throwError $ JobFailure {_exitCode = exitCode, _message = Nothing}

-- | The sink for the job's output, labelled with its kind.
getSink :: Job Sink
getSink = asks $ \env -> _printer env $ _kind env

-- | Stops the worker before returning, including on job failure or cancellation.
withBackgroundOutputWorker :: (Sink -> IO ()) -> Job a -> Job a
withBackgroundOutputWorker worker action = do
  sink <- getSink
  Catch.bracket
    (liftIO $ Async.async $ worker sink)
    (liftIO . Async.cancel)
    (const action)
