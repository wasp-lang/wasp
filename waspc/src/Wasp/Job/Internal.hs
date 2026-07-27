{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.Job.Internal
  ( Job,
    JobAction (..),
    JobError (..),
    JobEvent (..),
    JobEventData (..),
    JobOutputStream (..),
    JobKind (..),
    JobOutputEmitter,
    makeJob,
    runJob,
    emitJobOutput,
    requireExitSuccess,
    getJobOutputEmitter,
    emitJobOutputIO,
  )
where

import Control.Concurrent (Chan, writeChan)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import Data.Text (Text)
import System.Exit (ExitCode (..))

data Job = Job JobKind (JobAction ())

newtype JobAction a = JobAction
  { unJobAction :: ReaderT JobEnv (ExceptT JobError (ResourceT IO)) a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader JobEnv, MonadError JobError, MonadThrow, MonadCatch, MonadMask, MonadResource)

newtype JobError = JobFailed Int
  deriving (Eq, Show)

newtype JobEnv = JobEnv
  { jobOutputEmitter :: JobOutputEmitter
  }

newtype JobOutputEmitter = JobOutputEmitter (JobOutputStream -> Text -> IO ())

makeJob :: JobKind -> JobAction () -> Job
makeJob = Job

runJob :: Job -> Chan JobEvent -> IO ExitCode
runJob (Job jobKind action) chan = do
  result <-
    runResourceT $
      runExceptT $
        runReaderT (unJobAction action) JobEnv {jobOutputEmitter = outputEmitter}
  let exitCode = either jobErrorExitCode (const ExitSuccess) result
  writeChan chan $
    JobEvent
      { _eventData = JobExited exitCode,
        _jobKind = jobKind
      }
  return exitCode
  where
    outputEmitter =
      JobOutputEmitter $ \outputStream output ->
        writeChan chan $
          JobEvent
            { _eventData = JobOutput output outputStream,
              _jobKind = jobKind
            }

jobErrorExitCode :: JobError -> ExitCode
jobErrorExitCode (JobFailed exitCode) = ExitFailure exitCode

emitJobOutput :: JobOutputStream -> Text -> JobAction ()
emitJobOutput outputStream output = do
  outputEmitter <- getJobOutputEmitter
  liftIO $ emitJobOutputIO outputEmitter outputStream output

requireExitSuccess :: ExitCode -> JobAction ()
requireExitSuccess ExitSuccess = return ()
requireExitSuccess (ExitFailure exitCode) = throwError $ JobFailed exitCode

getJobOutputEmitter :: JobAction JobOutputEmitter
getJobOutputEmitter = asks jobOutputEmitter

emitJobOutputIO :: JobOutputEmitter -> JobOutputStream -> Text -> IO ()
emitJobOutputIO (JobOutputEmitter emit) = emit

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
