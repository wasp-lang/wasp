module Wasp.Job.Process (run, run_) where

import Control.Concurrent.Async (Concurrently (..))
import Control.Exception (IOException, bracket, finally, try)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import System.Exit (ExitCode)
import System.IO (Handle, hClose)
import qualified System.Info
import qualified System.Process as P
import Wasp.Job (Job, Sink, Stream (..), getSink, requireExitSuccess)

-- TODO(#4575):
--   Switch from System.Process to System.Process.Typed.

-- | Runs the process to completion and forwards its output to the job's sink.
-- A process whose stdin is 'P.CreatePipe' gets an empty stdin.
run :: P.CreateProcess -> Job ExitCode
run process = do
  sink <- getSink
  liftIO $ bracket start cleanUp (waitForExit sink)
  where
    start = P.createProcess process {P.std_out = P.CreatePipe, P.std_err = P.CreatePipe}

    waitForExit sink (stdinHandle, stdoutHandle, stderrHandle, processHandle) = do
      mapM_ hClose stdinHandle
      runConcurrently $
        Concurrently (forwardOutput sink Stdout stdoutHandle)
          *> Concurrently (forwardOutput sink Stderr stderrHandle)
          *> Concurrently (P.waitForProcess processHandle)

    cleanUp resources@(_, _, _, processHandle) =
      terminate processHandle `finally` closeHandles resources

    terminate processHandle =
      if System.Info.os == "mingw32"
        then P.terminateProcess processHandle
        else P.interruptProcessGroupOf processHandle

-- | Like 'run', but fails the job if the process exits with a nonzero code.
run_ :: P.CreateProcess -> Job ()
run_ process = run process >>= requireExitSuccess

forwardOutput :: Sink -> Stream -> Maybe Handle -> IO ()
forwardOutput _ _ Nothing = return ()
forwardOutput sink stream (Just handle) =
  runConduit $
    CB.sourceHandle handle .| CT.decodeUtf8Lenient .| CL.mapM_ (sink stream)

closeHandles :: (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle) -> IO ()
closeHandles (stdinHandle, stdoutHandle, stderrHandle, _) =
  mapM_ closeHandle [stdinHandle, stdoutHandle, stderrHandle]
  where
    closeHandle Nothing = return ()
    closeHandle (Just handle) = void (try (hClose handle) :: IO (Either IOException ()))
