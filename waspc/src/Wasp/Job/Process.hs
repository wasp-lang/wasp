module Wasp.Job.Process
  ( fromProc,
  )
where

import Control.Concurrent (forkIO)
import qualified Control.Concurrent.Async as Async
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import Data.Text (Text)
import System.Exit (ExitCode)
import System.IO (Handle, hClose)
import qualified System.Info
import qualified System.Process as P
import UnliftIO.Exception (bracket)
import Wasp.Job.Internal (Job (..))
import Wasp.Job.Printer (OutputKind (..))

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
