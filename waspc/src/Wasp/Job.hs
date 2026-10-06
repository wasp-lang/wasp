module Wasp.Job
  ( Job,
    Stream (..),
    Sink,
    runJob,
    emitJobOutput,
    failWithExitCode,
    requireExitSuccess,
    getSink,
    withBackgroundOutputWorker,
  )
where

import qualified Control.Concurrent.Async as Async
import qualified Control.Monad.Catch as Catch
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Text (Text)
import System.Exit (ExitCode (..))

type Job = ReaderT Sink (ExceptT JobFailure (ResourceT IO))

newtype JobFailure = JobFailure Int

data Stream = Stdout | Stderr deriving (Show, Eq, Ord)

-- | Receives a job's output. Jobs can call it from several threads at once.
type Sink = Stream -> Text -> IO ()

-- | Returns once the job has finished and its resources are released.
runJob :: Sink -> Job () -> IO ExitCode
runJob sink job =
  either jobFailureExitCode (const ExitSuccess)
    <$> runResourceT (runExceptT $ runReaderT job sink)

jobFailureExitCode :: JobFailure -> ExitCode
jobFailureExitCode (JobFailure exitCode) = ExitFailure exitCode

emitJobOutput :: Stream -> Text -> Job ()
emitJobOutput stream output = do
  sink <- getSink
  liftIO $ sink stream output

requireExitSuccess :: ExitCode -> Job ()
requireExitSuccess ExitSuccess = return ()
requireExitSuccess (ExitFailure exitCode) = failWithExitCode exitCode

failWithExitCode :: Int -> Job a
failWithExitCode = throwError . JobFailure

getSink :: Job Sink
getSink = ask

-- | Stops the worker before returning, including on job failure or cancellation.
withBackgroundOutputWorker :: (Sink -> IO ()) -> Job a -> Job a
withBackgroundOutputWorker worker action = do
  sink <- getSink
  Catch.bracket
    (liftIO $ Async.async $ worker sink)
    (liftIO . Async.cancel)
    (const action)
