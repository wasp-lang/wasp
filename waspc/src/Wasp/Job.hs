module Wasp.Job
  ( Job,
    run,
    race,
    OutputKind (..),
    emitOutput,
    captureOutput,
    onOutput,
    onProcessExit,
    fromProc,
    fromInteractiveProc,
    ProcessGroupDidNotStop (..),
    JobKind (..),
    prefixWith,
  )
where

import qualified Control.Concurrent.Async as Async
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), ask, local)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode)
import Wasp.Job.Internal (Job (..), JobEnv (..), runWithEnv, withSink)
import Wasp.Job.Printer (JobKind (..), OutputKind (..))
import qualified Wasp.Job.Printer as Printer
import Wasp.Job.Process (ProcessGroupDidNotStop (..), fromInteractiveProc, fromProc)

-- | Runs the job, printing its output to Wasp's own stdout and stderr, and
-- returns once it has finished.
run :: (MonadIO m) => Job a -> m a
run job = liftIO $ do
  printer <- Printer.newPrinter
  runWithEnv (JobEnv (Printer.printOutput printer) (const $ return ())) job

-- | Runs both jobs at the same time, until one of them finishes. Then it stops
-- the other one and returns the result of the first one.
race :: Job a -> Job b -> Job (Either a b)
race left right = Job $ ReaderT $ \env ->
  Async.race (runWithEnv env left) (runWithEnv env right)

emitOutput :: OutputKind -> Text -> Job ()
emitOutput outputKind output = Job $ do
  env <- ask
  liftIO $ _sink env Nothing outputKind output

-- | Collects all the output the job emits, from both stdout and stderr, in
-- the order it was emitted, instead of passing it on.
captureOutput :: Job a -> Job (a, Text)
captureOutput job = do
  chunksRef <- liftIO $ newIORef []
  let capture _ _ output = atomicModifyIORef' chunksRef $ \chunks -> (output : chunks, ())
  result <- withSink (const capture) job
  chunks <- liftIO $ readIORef chunksRef
  return (result, T.concat $ reverse chunks)

-- | Calls the given action every time the job emits output.
onOutput :: IO () -> Job a -> Job a
onOutput action = withSink $ \sink jobKind outputKind output ->
  action >> sink jobKind outputKind output

-- | Calls the given action as soon as a process that the job runs exits by
-- itself, with its exit code. That is before the processes it left behind are
-- stopped, and before all of its output is emitted.
onProcessExit :: (ExitCode -> IO ()) -> Job a -> Job a
onProcessExit action (Job job) = Job $ local addHook job
  where
    addHook env = env {_processExitHook = \exitCode -> action exitCode >> _processExitHook env exitCode}

-- | Prints the job's output with the job kind's prefix, e.g. "[Server]". If
-- 'prefixWith' calls are nested, the outermost one decides the prefix.
prefixWith :: JobKind -> Job a -> Job a
prefixWith jobKind = withSink $ \sink _ -> sink (Just jobKind)
