module Wasp.Job
  ( Job,
    proc,
    CreateJobProcess,
    setCwd,
    markInteractive,
    run,
    race,
    hotSwap,
    andThen,
    OutputType (..),
    emitOutput,
    captureOutput,
    observeOutput,
    onProcessExit,
    fromProc,
    npmBinProc,
    JobType (..),
    prefixWith,
  )
where

import Control.Monad.Extra (untilJustM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), ask, local)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Exit (ExitCode (..))
import qualified UnliftIO.Async as Async
import Wasp.Env (addEnvVarsOverride)
import Wasp.Job.Common (Job (..), JobEnv (..), JobType (..), OutputType (..), runWithEnv, withSink)
import Wasp.Job.CreateProcess (CreateJobProcess, markInteractive, proc, setCwd)
import qualified Wasp.Job.IO.PrefixedWriter as PrefixedWriter
import Wasp.Job.Process (fromProc)
import Wasp.Node.NpmBin (findNpmBin, getNpmBinEnvVars)

-- | Runs the job, printing its output to Wasp's own stdout and stderr, and
-- returns once it has finished.
run :: (MonadIO m) => Job a -> m a
run job = liftIO $ do
  prefixedWriter <- PrefixedWriter.newPrefixedWriter
  runWithEnv (JobEnv (PrefixedWriter.printOutput prefixedWriter) (const $ return ())) job

-- | Runs both jobs at the same time, until one of them finishes. Then it stops
-- the other one and returns the result of the first one.
race :: Job a -> Job b -> Job (Either a b)
race left right = Job $ ReaderT $ \env ->
  Async.race (runWithEnv env left) (runWithEnv env right)

-- | Runs a job, and keeps calling @produceReplacement@ until it returns a new
-- job that replaces it. Never returns.
hotSwap :: Job (Maybe (Job ())) -> Job () -> Job Void
hotSwap produceReplacement firstJob = runSwapping firstJob
  where
    runSwapping :: Job () -> Job Void
    runSwapping job = do
      nextJob <- waitForReplacement job
      runSwapping nextJob

    waitForReplacement :: Job () -> Job (Job ())
    waitForReplacement currentJob =
      Async.withAsync currentJob $ \currentJobAsync -> do
        Async.link currentJobAsync -- Rethrows the current job's exceptions.
        untilJustM produceReplacement

emitOutput :: OutputType -> Text -> Job ()
emitOutput outputType output = Job $ do
  env <- ask
  liftIO $ _sink env Nothing outputType output

-- | Collects all the output the job emits, from both stdout and stderr, in
-- the order it was emitted, as the monad's result.
captureOutput :: Job a -> Job (a, Text)
captureOutput job = do
  chunksRef <- liftIO $ newIORef []
  let capture _ _ output = atomicModifyIORef' chunksRef $ \chunks -> (output : chunks, ())
  result <- withSink (const capture) job
  chunks <- liftIO $ readIORef chunksRef
  return (result, T.concat $ reverse chunks)

-- | Calls @onOutput@ with every output the job emits, and then passes the
-- output on unchanged.
observeOutput :: (OutputType -> Text -> IO ()) -> Job a -> Job a
observeOutput onOutput job = withSink wrapOutputSink job
  where
    wrapOutputSink forwardOutput =
      \jobType outputType outputChunk -> do
        onOutput outputType outputChunk
        forwardOutput jobType outputType outputChunk

-- | Runs the second job only if the first one succeeds (like @&&@ in a shell),
-- and returns the exit code of the last one that ran.
andThen :: Job ExitCode -> Job ExitCode -> Job ExitCode
andThen firstJob secondJob =
  firstJob >>= \case
    ExitSuccess -> secondJob
    exitCode -> return exitCode

-- | Calls the given action as soon as a process that the job runs exits by
-- itself, with its exit code. That is before all of its output is emitted.
onProcessExit :: (ExitCode -> IO ()) -> Job a -> Job a
onProcessExit action (Job job) = Job $ local addHook job
  where
    addHook env = env {_processExitHook = \exitCode -> action exitCode >> _processExitHook env exitCode}

-- | A process that runs the npm package executable with the given name, found
-- with 'findNpmBin', in the given directory, with the env vars from
-- 'getNpmBinEnvVars'. Fails if there is no such executable.
npmBinProc :: Path' Abs (Dir d) -> String -> [String] -> Job CreateJobProcess
npmBinProc dir binName args =
  liftIO (findNpmBin dir binName) >>= \case
    Nothing ->
      fail $
        "Could not find `" <> binName <> "` in node_modules/.bin of " <> SP.fromAbsDir dir <> " or its parents."
    Just bin -> do
      npmBinEnvVars <- liftIO $ getNpmBinEnvVars dir

      return
        $ addEnvVarsOverride npmBinEnvVars
        $ setCwd dir
        $ proc bin args

-- | Prints the job's output with the job kind's prefix, e.g. "[Server]". If
-- 'prefixWith' calls are nested, the outermost one decides the prefix.
prefixWith :: JobType -> Job a -> Job a
prefixWith outerJobType = withSink $ \sink _innerJobType -> sink (Just outerJobType)
