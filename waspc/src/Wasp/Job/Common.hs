{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.Job.Common
  ( Job (..),
    JobType (..),
    OutputType (..),
    Sink,
    withSink,
    runWithSink,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (..), local)
import Data.Text (Text)

-- | An action that runs processes and emits their output.
newtype Job a = Job (ReaderT Sink IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Labels the output of a job, e.g. "[Server]".
data JobType = WebApp | Server | Db | Wasp deriving (Show, Eq, Ord, Bounded, Enum)

data OutputType = Stdout | Stderr deriving (Show, Eq, Ord)

-- | Receives a job's output, labeled with the kind of the job it comes from,
-- if any. Jobs can call it from several threads at once.
type Sink = Maybe JobType -> OutputType -> Text -> IO ()

withSink :: (Sink -> Sink) -> Job a -> Job a
withSink modifySink (Job job) = Job $ local modifySink job

runWithSink :: Sink -> Job a -> IO a
runWithSink sink (Job job) = runReaderT job sink
