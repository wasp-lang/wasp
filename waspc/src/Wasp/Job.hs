module Wasp.Job
  ( Job,
    Output (..),
    emit,
    getOutputHandle,
    fromCallback,
    runWith,
  )
where

import Control.Concurrent.Async (asyncWithUnmask, cancel, waitCatchSTM)
import Control.Concurrent.STM (atomically, newTQueueIO, orElse, readTQueue, writeTQueue)
import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Conduit (ConduitT, bracketP, fuseUpstream, runConduitRes, yield)
import qualified Data.Conduit.List as CL
import Data.Text (Text)
import System.IO (Handle, stderr, stdout)
import Wasp.Process (OutputStream (..))

-- | A job streams the output of the work it does, e.g. of the processes it
-- runs, and ends with a result, usually the exit code of its last process.
--
-- Jobs compose like any other conduit: run them in sequence with '>>=',
-- transform their output with 'Data.Conduit.fuseUpstream', and consume it
-- with the runners in "Wasp.Job.Output".
type Job = ConduitT () Output (ResourceT IO)

data Output = Output OutputStream Text
  deriving (Show, Eq)

emit :: OutputStream -> Text -> Job ()
emit stream text = yield $ Output stream text

-- | Wasp's own handle for printing output from the given stream.
getOutputHandle :: OutputStream -> Handle
getOutputHandle Stdout = stdout
getOutputHandle Stderr = stderr

-- | Runs the action in a background thread and streams the output it emits,
-- in order. Returns the action's result after streaming all of its output.
-- If the job is stopped before that, the action is cancelled.
--
-- NOTE: stm-conduit's @Data.Conduit.Async.gatherFrom@ does the same, but
-- with a bounded queue. We use an unbounded one, so a slow consumer never
-- blocks a process that is writing output.
fromCallback :: ((OutputStream -> Text -> IO ()) -> IO a) -> Job a
fromCallback action = do
  queue <- liftIO newTQueueIO
  let emitToQueue stream text = atomically $ writeTQueue queue $ Output stream text
  bracketP
    (asyncWithUnmask $ \unmask -> unmask $ action emitToQueue)
    cancel
    (streamUntilDone queue)
  where
    streamUntilDone queue worker = do
      -- Output is queued before the worker finishes, so reading the queue
      -- first guarantees nothing is left in it once we see the result.
      next <- liftIO $ atomically $ (Left <$> readTQueue queue) `orElse` (Right <$> waitCatchSTM worker)
      case next of
        Left output -> yield output >> streamUntilDone queue worker
        Right (Left exception) -> liftIO $ throwIO exception
        Right (Right result) -> return result

-- | Runs the job to completion, passing its output to the callback.
runWith :: (OutputStream -> Text -> IO ()) -> Job a -> IO a
runWith callback job =
  runConduitRes $
    job `fuseUpstream` CL.mapM_ (\(Output stream text) -> liftIO $ callback stream text)
