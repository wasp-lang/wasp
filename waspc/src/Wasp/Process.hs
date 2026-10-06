module Wasp.Process
  ( InputMode (..),
    OutputStream (..),
    ProcessGroupDidNotStop (..),
    run,
    runUntil,
  )
where

import Control.Concurrent (newEmptyMVar, putMVar, readMVar, threadDelay)
import Control.Concurrent.Async (Concurrently (..), concurrently_, race, runConcurrently, withAsync)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (STM, atomically, orElse, retry)
import Control.Exception (Exception (displayException), IOException, bracketOnError, finally, onException, throwIO, try)
import Control.Monad (unless, void)
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.Text
import System.Exit (ExitCode)
import System.IO (Handle, hClose)
import qualified System.Process as P
import System.Timeout (timeout)
import qualified Wasp.Process.System as System
import Wasp.Util (secondsToMicroSeconds)

data InputMode = InheritTerminal | NoInput
  deriving (Show, Eq)

data OutputStream = Stdout | Stderr deriving (Show, Eq)

data ProcessGroupDidNotStop = ProcessGroupDidNotStop
  deriving (Show, Eq)

instance Exception ProcessGroupDidNotStop where
  displayException _ = "Could not stop the subprocess group. A child process may still be running."

-- | Runs the command to completion and forwards all output before returning.
run :: InputMode -> P.CreateProcess -> (OutputStream -> Data.Text.Text -> IO ()) -> IO ExitCode
run = runUntil retry -- 'retry' blocks forever, so the command is never asked to stop.

-- | Like 'run', but also stops the command once the given transaction
-- succeeds. Output that the command writes while stopping is still forwarded.
runUntil :: STM () -> InputMode -> P.CreateProcess -> (OutputStream -> Data.Text.Text -> IO ()) -> IO ExitCode
runUntil stopRequested inputMode process emit =
  bracketOnError start cleanUp $ \resources@((_, stdoutHandle, stderrHandle, processHandle), processGroup) -> do
    stopped <- newEmptyMVar
    let forwardAllOutput =
          concurrently_ (forwardOutput Stdout stdoutHandle) (forwardOutput Stderr stderrHandle)
    -- A process that left the group can keep the output pipes open, so we
    -- only wait a bit for the remaining output after stopping the group.
    let outputDrainDeadline = readMVar stopped >> threadDelay outputDrainTimeoutMicroseconds
    exitCode <-
      withAsync (P.waitForProcess processHandle) $ \rootExit ->
        runConcurrently $
          Concurrently (void $ race forwardAllOutput outputDrainDeadline)
            *> Concurrently (waitForRootAndStop processHandle processGroup rootExit <* putMVar stopped ())
    closeHandles $ fst resources
    return exitCode
  where
    configuredProcess = case inputMode of
      NoInput -> System.configureIsolatedProcess process
      InheritTerminal ->
        process
          { P.create_group = False,
            P.use_process_jobs = False,
            P.std_in = P.Inherit,
            P.std_out = P.CreatePipe,
            P.std_err = P.CreatePipe
          }

    start = do
      resources@(_, _, _, processHandle) <- P.createProcess configuredProcess
      processGroup <-
        ( case inputMode of
            NoInput -> P.getPid processHandle
            InheritTerminal -> return Nothing
        )
          `onException` emergencyCleanUp resources
      return (resources, processGroup)

    cleanUp (resources@(_, _, _, processHandle), processGroup) =
      withAsync (P.waitForProcess processHandle) (stop processHandle processGroup)
        `finally` closeHandles resources

    stop processHandle processGroup rootExit = case inputMode of
      NoInput -> ensureGroupStopped processHandle rootExit processGroup
      InheritTerminal ->
        P.getProcessExitCode processHandle >>= \case
          Just _ -> return ()
          Nothing -> P.terminateProcess processHandle

    ensureGroupStopped processHandle rootExit processGroup = do
      stopped <- System.stopProcessGroup processHandle rootExit processGroup
      unless stopped $ throwIO ProcessGroupDidNotStop

    waitForRootAndStop processHandle processGroup rootExit = do
      atomically $ void (Async.waitCatchSTM rootExit) `orElse` stopRequested
      stop processHandle processGroup rootExit
      Async.wait rootExit

    forwardOutput _ Nothing = return ()
    forwardOutput stream (Just handle) =
      runConduit $
        CB.sourceHandle handle .| CT.decodeUtf8Lenient .| CL.mapM_ (emit stream)

emergencyCleanUp :: (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle) -> IO ()
emergencyCleanUp resources@(_, _, _, processHandle) =
  ( do
      P.terminateProcess processHandle
      void $ timeout System.hardStopTimeoutMicroseconds $ P.waitForProcess processHandle
  )
    `finally` closeHandles resources

closeHandles :: (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle) -> IO ()
closeHandles (stdinHandle, stdoutHandle, stderrHandle, _) =
  mapM_ closeHandle [stdinHandle, stdoutHandle, stderrHandle]
  where
    closeHandle Nothing = return ()
    closeHandle (Just handle) = void (try (hClose handle) :: IO (Either IOException ()))

outputDrainTimeoutMicroseconds :: Int
outputDrainTimeoutMicroseconds = secondsToMicroSeconds 1
