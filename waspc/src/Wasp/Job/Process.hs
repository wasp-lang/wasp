{-# LANGUAGE CPP #-}

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
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose)
import qualified System.Process as P
import UnliftIO.Exception (bracket)
import Wasp.Job.Common (Job (..), OutputType (..))
import Wasp.Job.CreateProcess (CreateJobProcess, asCreateProcess)
#if !defined(mingw32_HOST_OS)
import Data.Functor ((<&>))
import Data.Maybe (mapMaybe)
import System.IO.Error (tryIOError)
import qualified System.Posix.Signals as Signals
import Text.Read (readMaybe)
#endif

-- | Runs the process to completion, emitting its stdout and stderr as the job's
-- output, and returns its exit code.
-- Makes sure to stop the process (and its descendants on *nix) if the job is
-- stopped before the process finishes.
fromProc :: CreateJobProcess -> Job ExitCode
fromProc jobProcess = Job $ do
  process <- liftIO $ asCreateProcess jobProcess
  sink <- ask
  liftIO $ bracket (start process) cleanUp (waitForExit $ sink Nothing)
  where
    start process = P.createProcess (process {P.std_out = P.CreatePipe, P.std_err = P.CreatePipe})

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

-- | Stops the process, and on *nix also all its descendants.
-- We can't count on the process to stop its descendants: e.g. npm and npx run
-- commands with `sh -c`, and dash (`sh` on Debian and Ubuntu) doesn't forward
-- the signal to its child.
-- We don't signal the process group instead: Wasp doesn't start processes in
-- their own group, so that group would also include Wasp itself, its other
-- jobs, and whatever started Wasp. And we don't start each process in its own
-- group either: an interactive process would then get stopped when it reads
-- from the terminal, and Ctrl+C in the terminal would no longer reach it.
-- Windows has no signals, so there we terminate just the process.
terminate :: P.ProcessHandle -> IO ()
#if defined(mingw32_HOST_OS)
terminate = P.terminateProcess
#else
terminate processHandle = P.getPid processHandle >>= mapM_ interruptProcessTree

-- | Sends SIGINT to the process and all its descendants, which is what Ctrl+C
-- in the terminal would reach.
interruptProcessTree :: P.Pid -> IO ()
interruptProcessTree rootPid = do
  -- We list the descendants before sending any signal, because once a process
  -- exits, its children get a new parent. We miss descendants started after
  -- this, but processes rarely start new ones while they're being stopped.
  descendantPids <- getDescendantPids rootPid
  -- Some of the processes might have exited in the meantime, so we ignore errors.
  mapM_ (tryIOError . Signals.signalProcess Signals.sigINT) (rootPid : descendantPids)

-- | Lists the descendants of the process with `ps`.
-- If `ps` fails (e.g. it isn't installed, as in Debian's slim images), we list
-- none, and only signal the process itself.
getDescendantPids :: P.Pid -> IO [P.Pid]
getDescendantPids rootPid =
  tryIOError (P.readProcessWithExitCode "ps" ["-A", "-o", "pid=", "-o", "ppid="] "") <&> \case
    Right (ExitSuccess, psOutput, _) -> descendantsOf rootPid $ mapMaybe parsePsLine $ lines psOutput
    _ -> []
  where
    parsePsLine line = case mapM readMaybe (words line) of
      Just [pid, parentPid] -> Just (pid, parentPid)
      _ -> Nothing

    descendantsOf pid pidsWithParents =
      concatMap
        (\childPid -> childPid : descendantsOf childPid pidsWithParents)
        [childPid | (childPid, parentPid) <- pidsWithParents, parentPid == pid]
#endif

forwardOutput :: (Text -> IO ()) -> Maybe Handle -> IO ()
forwardOutput _ Nothing = return ()
forwardOutput emit (Just handle) =
  runConduit $
    CB.sourceHandle handle .| CT.decodeUtf8Lenient .| CL.mapM_ emit
