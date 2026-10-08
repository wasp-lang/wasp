{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.Job.Common
  ( Job (..),
    JobEnv (..),
    JobType (..),
    OutputType (..),
    Sink,
    withSink,
    runWithEnv,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (..), local)
import Data.Text (Text)
import System.Exit (ExitCode)

-- | An action that runs processes and emits their output.
newtype Job a = Job (ReaderT JobEnv IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

data JobEnv = JobEnv
  { _sink :: Sink,
    _processExitHook :: ExitCode -> IO ()
  }

-- | Labels the output of a job, e.g. "[Server]".
data JobType = WebApp | Server | Db | Wasp deriving (Show, Eq, Ord, Bounded, Enum)

data OutputType = Stdout | Stderr deriving (Show, Eq, Ord)

-- | Receives a job's output, labeled with the kind of the job it comes from,
-- if any. Jobs can call it from several threads at once.
type Sink = Maybe JobType -> OutputType -> Text -> IO ()

withSink :: (Sink -> Sink) -> Job a -> Job a
withSink modifySink (Job job) = Job $ local (\env -> env {_sink = modifySink $ _sink env}) job

runWithEnv :: JobEnv -> Job a -> IO a
runWithEnv env (Job job) = runReaderT job env
