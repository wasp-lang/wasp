{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.Job
  ( Job,
    run,
    race,
    OutputKind (..),
    emitOutput,
    captureOutput,
    onOutput,
    failOnExitFailure,
    fromProc,
    fromInteractiveProc,
    JobKind (..),
    prefixWith,
  )
where

import qualified Control.Concurrent.Async as Async
import Control.Monad.Except (ExceptT (..), MonadError (throwError), liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), ask, local)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import qualified System.Process as P
import Wasp.Job.Printer (JobKind (..), OutputKind (..))
import qualified Wasp.Job.Printer as Printer
import qualified Wasp.Job.Process as JobProcess

-- | An action that runs processes and emits their output. It can fail with an
-- error of type @e@.
newtype Job e a = Job (ReaderT Sink (ExceptT e IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Receives a job's output, labeled with the kind of the job it comes from,
-- if any. Jobs can call it from several threads at once.
type Sink = Maybe JobKind -> OutputKind -> Text -> IO ()

-- | Runs the job, printing its output to Wasp's own stdout and stderr, and
-- returns once it has finished.
run :: (MonadIO m, MonadError e m) => Job e a -> m a
run job = do
  printer <- liftIO Printer.newPrinter
  liftIO (runWithSink (Printer.printOutput printer) job) >>= liftEither

-- | Runs both jobs at the same time, until one of them finishes. Then it stops
-- the other one and returns the result (or failure) of the first one.
race :: Job e a -> Job e b -> Job e (Either a b)
race left right = Job $ ReaderT $ \sink ->
  ExceptT $
    either (fmap Left) (fmap Right)
      <$> Async.race (runWithSink sink left) (runWithSink sink right)

emitOutput :: OutputKind -> Text -> Job e ()
emitOutput outputKind output = Job $ do
  sink <- ask
  liftIO $ sink Nothing outputKind output

-- | Collects all the output the job emits, from both stdout and stderr, in
-- the order it was emitted, instead of passing it on.
captureOutput :: Job e a -> Job e (a, Text)
captureOutput job = do
  chunksRef <- liftIO $ newIORef []
  let capture _ _ output = atomicModifyIORef' chunksRef $ \chunks -> (output : chunks, ())
  result <- withSink (const capture) job
  chunks <- liftIO $ readIORef chunksRef
  return (result, T.concat $ reverse chunks)

-- | Calls the given action every time the job emits output.
onOutput :: IO () -> Job e a -> Job e a
onOutput action = withSink $ \sink jobKind outputKind output ->
  action >> sink jobKind outputKind output

-- | Fails the job with the error that the given function returns for its exit
-- code, unless it exited successfully.
failOnExitFailure :: (Int -> e) -> Job e ExitCode -> Job e ()
failOnExitFailure toError job =
  job >>= \case
    ExitSuccess -> return ()
    ExitFailure code -> Job $ throwError $ toError code

-- | Prints the job's output with the job kind's prefix, e.g. "[Server]". If
-- 'prefixWith' calls are nested, the outermost one decides the prefix.
prefixWith :: JobKind -> Job e a -> Job e a
prefixWith jobKind = withSink $ \sink _ -> sink (Just jobKind)

-- | Runs the process to completion in its own process group (a job object on
-- Windows), with an empty stdin. Emits its stdout and stderr as the job's
-- output, and returns its exit code once all of it has been emitted.
--
-- Once the process exits, or the job is stopped, any process left in its group
-- is stopped too. If they don't stop in time, this throws
-- 'JobProcess.ProcessGroupDidNotStop'.
fromProc :: P.CreateProcess -> Job e ExitCode
fromProc = runProcess JobProcess.Isolated

-- | Like 'fromProc', but the process reads from Wasp's stdin. For that, it
-- stays in Wasp's process group, so stopping the job only stops the process
-- itself, and not the processes it started.
fromInteractiveProc :: P.CreateProcess -> Job e ExitCode
fromInteractiveProc = runProcess JobProcess.Interactive

runProcess :: JobProcess.Interactivity -> P.CreateProcess -> Job e ExitCode
runProcess interactivity process = Job $ do
  sink <- ask
  liftIO $ JobProcess.run interactivity (sink Nothing) process

withSink :: (Sink -> Sink) -> Job e a -> Job e a
withSink modifySink (Job job) = Job $ local modifySink job

runWithSink :: Sink -> Job e a -> IO (Either e a)
runWithSink sink (Job job) = runExceptT $ runReaderT job sink
