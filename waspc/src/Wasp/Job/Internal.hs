{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.Job.Internal
  ( Job (..),
    Sink,
    withSink,
    runWithSink,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (..), local)
import Data.Text (Text)
import Wasp.Job.Printer (JobKind, OutputKind)

-- | An action that runs processes and emits their output.
newtype Job a = Job (ReaderT Sink IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Receives a job's output, labeled with the kind of the job it comes from,
-- if any. Jobs can call it from several threads at once.
type Sink = Maybe JobKind -> OutputKind -> Text -> IO ()

withSink :: (Sink -> Sink) -> Job a -> Job a
withSink modifySink (Job job) = Job $ local modifySink job

runWithSink :: Sink -> Job a -> IO a
runWithSink sink (Job job) = runReaderT job sink
