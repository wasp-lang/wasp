module Wasp.Job
  ( Job,
    proc,
    CreateJobProcess,
    setCwd,
    markInteractive,
    addEnvVarsOverride,
    run,
    runCapturingOutput,
    runCapturingToolOutput,
    race,
    andThen,
    emitOutput,
    captureOutput,
    observeOutput,
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
import System.Exit (ExitCode (..))
import Wasp.Job.Common (Job (..), JobType (..), runWithSink, withSink)
import Wasp.Job.CreateProcess (CreateJobProcess, addEnvVarsOverride, markInteractive, proc, setCwd)
import qualified Wasp.Job.IO.PrefixedWriter as PrefixedWriter
import Wasp.Job.Process (fromProc)
import Wasp.Problem (ToolOutput (..))
import Wasp.Util.IO.Process (OutputStream (..), WriteOutput)

-- | Runs the job, writing its output out with the given function, and returns
-- once it has finished.
--
-- The function receives the output that came from a job with a 'JobType'
-- already prefixed with it. Each run starts its prefixes from scratch.
run :: (MonadIO m) => WriteOutput -> Job a -> m a
run writeOutput job = liftIO $ do
  prefixedWriter <- PrefixedWriter.newPrefixedWriter
  runWithSink (PrefixedWriter.prefixOutput prefixedWriter writeOutput) job

-- | Runs the job and returns everything it emitted together with its result
-- (see 'captureOutput'). Nothing is left to write out, so it takes no
-- function for that.
runCapturingOutput :: (MonadIO m) => Job a -> m (a, Text)
runCapturingOutput = liftIO . runWithSink discardOutput . captureOutput
  where
    discardOutput _ _ _ = return ()

-- | Runs a tool without showing what it prints. If it fails, returns what it
-- printed. The name is what the user knows the tool by (e.g. "npm install").
runCapturingToolOutput :: (MonadIO m) => String -> Job ExitCode -> m (Either ToolOutput ())
runCapturingToolOutput toolName job = do
  (exitCode, output) <- runCapturingOutput job
  return $ case exitCode of
    ExitSuccess -> Right ()
    ExitFailure code -> Left ToolOutput {command = toolName, exitCode = code, output = T.unpack $ T.strip output}

-- | Runs both jobs at the same time, until one of them finishes. Then it stops
-- the other one and returns the result of the first one.
race :: Job a -> Job b -> Job (Either a b)
race left right = Job $ ReaderT $ \sink ->
  Async.race (runWithSink sink left) (runWithSink sink right)

emitOutput :: OutputStream -> Text -> Job ()
emitOutput outputStream output = Job $ do
  sink <- ask
  liftIO $ sink Nothing outputStream output

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
observeOutput :: (OutputStream -> Text -> IO ()) -> Job a -> Job a
observeOutput onOutput job = withSink wrapOutputSink job
  where
    wrapOutputSink forwardOutput =
      \jobType outputStream outputChunk -> do
        onOutput outputStream outputChunk
        forwardOutput jobType outputStream outputChunk

-- | Runs the second job only if the first one succeeds (like @&&@ in a shell),
-- and returns the exit code of the last one that ran.
andThen :: Job ExitCode -> Job ExitCode -> Job ExitCode
andThen firstJob secondJob =
  firstJob >>= \case
    ExitSuccess -> secondJob
    exitCode -> return exitCode

-- | Prints the job's output with the job kind's prefix, e.g. "[Server]". If
-- 'prefixWith' calls are nested, the outermost one decides the prefix.
prefixWith :: JobType -> Job a -> Job a
prefixWith outerJobType = withSink $ \sink _innerJobType -> sink (Just outerJobType)
