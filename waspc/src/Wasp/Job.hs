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

import qualified Control.Concurrent.Async as Async
import Control.Monad.Except (ExceptT (..), MonadError (throwError), liftEither)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), ask)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import Wasp.Job.Internal (Job (..), runWithSink, withSink)
import Wasp.Job.Printer (JobKind (..), OutputKind (..))
import qualified Wasp.Job.Printer as Printer
import Wasp.Job.Process (fromProc)

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
