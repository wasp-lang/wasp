module Wasp.Job
  ( Job,
    proc,
    CreateJobProcess (..),
    setCwd,
    markInteractive,
    run,
    race,
    OutputType (..),
    emitOutput,
    captureOutput,
    onOutput,
    fromProc,
    JobType (..),
    prefixWith,
  )
where

import qualified Control.Concurrent.Async as Async
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), ask)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Wasp.Job.Common (Job (..), JobType (..), OutputType (..), runWithSink, withSink)
import Wasp.Job.CreateProcess (CreateJobProcess (..), cwd, markInteractive, proc, setCwd)
import qualified Wasp.Job.Printer as Printer
import Wasp.Job.Process (fromProc)

-- | Runs the job, printing its output to Wasp's own stdout and stderr, and
-- returns once it has finished.
run :: (MonadIO m) => Job a -> m a
run job = liftIO $ do
  printer <- Printer.newPrinter
  runWithSink (Printer.printOutput printer) job

-- | Runs both jobs at the same time, until one of them finishes. Then it stops
-- the other one and returns the result of the first one.
race :: Job a -> Job b -> Job (Either a b)
race left right = Job $ ReaderT $ \sink ->
  Async.race (runWithSink sink left) (runWithSink sink right)

emitOutput :: OutputType -> Text -> Job ()
emitOutput outputType output = Job $ do
  sink <- ask
  liftIO $ sink Nothing outputType output

-- | Collects all the output the job emits, from both stdout and stderr, in
-- the order it was emitted, as the monad's result.
captureOutput :: Job a -> Job (a, Text)
captureOutput job = do
  chunksRef <- liftIO $ newIORef []
  let capture _ _ output = atomicModifyIORef' chunksRef $ \chunks -> (output : chunks, ())
  result <- withSink (const capture) job
  chunks <- liftIO $ readIORef chunksRef
  return (result, T.concat $ reverse chunks)

-- | Calls the given action every time the job emits output.
onOutput :: IO () -> Job a -> Job a
onOutput action = withSink $ \sink jobType outputType output ->
  action >> sink jobType outputType output

-- | Prints the job's output with the job kind's prefix, e.g. "[Server]". If
-- 'prefixWith' calls are nested, the outermost one decides the prefix.
prefixWith :: JobType -> Job a -> Job a
prefixWith outerJobType = withSink $ \sink _innerJobType -> sink (Just outerJobType)
