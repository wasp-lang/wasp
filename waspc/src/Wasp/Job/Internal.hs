{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.Job.Internal
  ( Job,
    JobAction,
    JobEvent (..),
    JobEventData (..),
    JobOutputStream (..),
    JobKind (..),
    JobOutputSink,
    makeJob,
    runJob,
    emitJobOutput,
    failWithExitCode,
    requireExitSuccess,
    getJobOutputSink,
    writeJobOutput,
  )
where

import Control.Concurrent (Chan, writeChan)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import Data.Text (Text)
import System.Exit (ExitCode (..))

data Job = Job JobKind (JobAction ())

newtype JobAction a = JobAction
  { unJobAction :: ReaderT JobOutputSink (ExceptT JobFailure (ResourceT IO)) a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader JobOutputSink, MonadError JobFailure, MonadThrow, MonadCatch, MonadMask, MonadResource)

newtype JobFailure = JobFailure Int

newtype JobOutputSink = JobOutputSink
  { writeJobOutput :: JobOutputStream -> Text -> IO ()
  }

data JobEvent = JobEvent
  { _eventData :: JobEventData,
    _jobKind :: JobKind
  }
  deriving (Show)

data JobEventData
  = JobOutput Text JobOutputStream
  | JobExited ExitCode
  deriving (Show)

data JobOutputStream = Stdout | Stderr deriving (Show, Eq)

data JobKind = WebApp | Server | Db | Wasp deriving (Show, Eq, Ord, Bounded, Enum)

makeJob :: JobKind -> JobAction () -> Job
makeJob = Job

runJob :: Job -> Chan JobEvent -> IO ExitCode
runJob (Job jobKind action) chan = do
  result <-
    runResourceT $
      runExceptT $
        runReaderT (unJobAction action) outputSink
  let exitCode = either jobFailureExitCode (const ExitSuccess) result
  emitEvent $ JobExited exitCode
  return exitCode
  where
    outputSink = JobOutputSink $ \outputStream output -> emitEvent $ JobOutput output outputStream
    emitEvent eventData =
      writeChan chan $
        JobEvent
          { _eventData = eventData,
            _jobKind = jobKind
          }

jobFailureExitCode :: JobFailure -> ExitCode
jobFailureExitCode (JobFailure exitCode) = ExitFailure exitCode

emitJobOutput :: JobOutputStream -> Text -> JobAction ()
emitJobOutput outputStream output = do
  outputSink <- getJobOutputSink
  liftIO $ writeJobOutput outputSink outputStream output

requireExitSuccess :: ExitCode -> JobAction ()
requireExitSuccess ExitSuccess = return ()
requireExitSuccess (ExitFailure exitCode) = failWithExitCode exitCode

failWithExitCode :: Int -> JobAction a
failWithExitCode = throwError . JobFailure

getJobOutputSink :: JobAction JobOutputSink
getJobOutputSink = ask
