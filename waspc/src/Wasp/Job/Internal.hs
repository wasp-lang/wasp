{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.Job.Internal
  ( Job (..),
    Sink,
    withSink,
    runWithSink,
  )
where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (..), local)
import Data.Text (Text)
import Wasp.Job.Printer (JobKind, OutputKind)

-- | An action that runs processes and emits their output. It can fail with an
-- error of type @e@.
newtype Job e a = Job (ReaderT Sink (ExceptT e IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Receives a job's output, labeled with the kind of the job it comes from,
-- if any. Jobs can call it from several threads at once.
type Sink = Maybe JobKind -> OutputKind -> Text -> IO ()

withSink :: (Sink -> Sink) -> Job e a -> Job e a
withSink modifySink (Job job) = Job $ local modifySink job

runWithSink :: Sink -> Job e a -> IO (Either e a)
runWithSink sink (Job job) = runExceptT $ runReaderT job sink
