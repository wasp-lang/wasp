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
    JobKind (..),
    prefixWith,
  )
where

import Control.Concurrent (forkIO)
import qualified Control.Concurrent.Async as Async
import Control.Monad (void)
import Control.Monad.Except (ExceptT (..), MonadError (throwError), liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), ask, local)
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose)
import qualified System.Info
import qualified System.Process as P
import UnliftIO.Exception (bracket)
import Wasp.Job.Printer (JobKind (..), OutputKind (..))
import qualified Wasp.Job.Printer as Printer

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

-- | Runs the process to completion, emitting its stdout and stderr as the
-- job's output, and returns its exit code. A process whose stdin is
-- 'P.CreatePipe' gets an empty stdin.
-- Makes sure to terminate the process (or process group on *nix) if the job is
-- stopped before the process finishes.
fromProc :: P.CreateProcess -> Job e ExitCode
fromProc process = Job $ do
  sink <- ask
  liftIO $ bracket start cleanUp (waitForExit $ sink Nothing)
  where
    start = P.createProcess process {P.std_out = P.CreatePipe, P.std_err = P.CreatePipe}

    waitForExit emit (stdinHandle, stdoutHandle, stderrHandle, processHandle) = do
      mapM_ hClose stdinHandle
      Async.runConcurrently $
        Async.Concurrently (forwardOutput (emit Stdout) stdoutHandle)
          *> Async.Concurrently (forwardOutput (emit Stderr) stderrHandle)
          *> Async.Concurrently (P.waitForProcess processHandle)

    cleanUp (_, _, _, processHandle) = do
      terminate processHandle
      -- Reaps the process once it exits, without making the job wait for it.
      void $ forkIO $ void $ P.waitForProcess processHandle

    -- NOTE(shayne): On *nix, we use interruptProcessGroupOf instead of terminateProcess because many
    -- processes we run will spawn child processes, which themselves may spawn child processes.
    -- We want to ensure the entire process chain is stopped.
    -- We are limiting support of this to *nix only now, as Windows requires create_group=True
    -- but that surfaces an issue where a new process group that needs stdin but is started as a
    -- background process gets terminated, appearing to hang.
    -- Ref: https://stackoverflow.com/questions/61856063/spawning-a-process-with-create-group-true-set-pgid-hangs-when-starting-docke
    terminate processHandle =
      if System.Info.os == "mingw32"
        then P.terminateProcess processHandle
        else P.interruptProcessGroupOf processHandle

forwardOutput :: (Text -> IO ()) -> Maybe Handle -> IO ()
forwardOutput _ Nothing = return ()
forwardOutput emit (Just handle) =
  runConduit $
    CB.sourceHandle handle .| CT.decodeUtf8Lenient .| CL.mapM_ emit

withSink :: (Sink -> Sink) -> Job e a -> Job e a
withSink modifySink (Job job) = Job $ local modifySink job

runWithSink :: Sink -> Job e a -> IO (Either e a)
runWithSink sink (Job job) = runExceptT $ runReaderT job sink
