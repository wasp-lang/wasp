module Wasp.Process
  ( InputMode (..),
    OutputStream (..),
    ProcessGroupDidNotStop (..),
    run,
  )
where

import Control.Concurrent.Async (Concurrently (..), runConcurrently, withAsync)
import Control.Exception (Exception (displayException), IOException, bracket, finally, onException, throwIO, try)
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

data InputMode = InheritTerminal | NoInput
  deriving (Show, Eq)

data OutputStream = Stdout | Stderr deriving (Show, Eq)

data ProcessGroupDidNotStop = ProcessGroupDidNotStop
  deriving (Show, Eq)

instance Exception ProcessGroupDidNotStop where
  displayException _ = "Could not stop the subprocess group. A child process may still be running."

-- | Runs the command to completion and forwards all output before returning.
run :: InputMode -> P.CreateProcess -> (OutputStream -> Data.Text.Text -> IO ()) -> IO ExitCode
run inputMode process emit =
  bracket start cleanUp $ \((_, stdoutHandle, stderrHandle, processHandle), _) ->
    runConcurrently $
      Concurrently (forwardOutput Stdout stdoutHandle)
        *> Concurrently (forwardOutput Stderr stderrHandle)
        *> Concurrently (P.waitForProcess processHandle)
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
      ( case inputMode of
          NoInput -> withAsync (P.waitForProcess processHandle) $ \rootExit -> do
            stopped <- System.stopProcessGroup processHandle rootExit processGroup
            unless stopped $ throwIO ProcessGroupDidNotStop
          InheritTerminal -> do
            P.getProcessExitCode processHandle >>= \case
              Just _ -> return ()
              Nothing -> P.terminateProcess processHandle
      )
        `finally` closeHandles resources

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
