{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wasp.Job.Process
  ( runProcessAsJob,
    runNodeCommandAsJob,
    runNodeCommandAsJobWithExtraEnv,
    runNodeCommandAsJobWithExtraEnvAndStdin,
  )
where

import Control.Concurrent (writeChan)
import Control.Concurrent.Async (Concurrently (..))
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Process as CP
import Data.Text.Encoding (decodeUtf8)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import qualified System.Process as P
import UnliftIO.Exception (bracket)
#if !defined(mingw32_HOST_OS)
import Data.Maybe (mapMaybe)
import System.IO.Error (tryIOError)
import qualified System.Posix.Signals as Signals
import Text.Read (readMaybe)
#endif
import qualified Wasp.Job as J

-- TODO:
--   Switch from Data.Conduit.Process to Data.Conduit.Process.Typed.
--   It is a new module meant to replace Data.Conduit.Process which is about to become deprecated.

-- | Same as 'runProcessAsJobWithStdin', with stdin inherited.
runProcessAsJob :: P.CreateProcess -> J.JobType -> J.Job
runProcessAsJob = runProcessAsJobWithStdin CP.Inherited

-- | Runs a given process while streaming its stderr and stdout to provided channel.
--   The type of the first argument decides the process' stdin, e.g. 'CP.Inherited' for Wasp's own,
--   or 'CP.ClosedStream' for one that is already at end-of-input.
--   Returns exit code of the process once it finishes, and also sends it to the channel.
--   Makes sure to stop the process if exception occurs.
runProcessAsJobWithStdin :: forall stdin. (CP.InputSource stdin) => stdin -> P.CreateProcess -> J.JobType -> J.Job
runProcessAsJobWithStdin _stdin process jobType chan =
  bracket
    (CP.streamingProcess process)
    (\(_, _, _, sph) -> terminateStreamingProcess sph)
    runStreamingProcessAsJob
  where
    runStreamingProcessAsJob (_ :: stdin, stdoutStream, stderrStream, processHandle) = do
      let forwardStdoutToChan =
            runConduit $
              stdoutStream
                .| CL.mapM_
                  ( \bs ->
                      writeChan chan $
                        J.JobMessage
                          { J._data = J.JobOutput (decodeUtf8 bs) J.Stdout,
                            J._jobType = jobType
                          }
                  )

      let forwardStderrToChan =
            runConduit $
              stderrStream
                .| CL.mapM_
                  ( \bs ->
                      writeChan chan $
                        J.JobMessage
                          { J._data = J.JobOutput (decodeUtf8 bs) J.Stderr,
                            J._jobType = jobType
                          }
                  )

      exitCode <-
        runConcurrently $
          Concurrently forwardStdoutToChan
            *> Concurrently forwardStderrToChan
            *> Concurrently (CP.waitForStreamingProcess processHandle)

      writeChan chan $
        J.JobMessage
          { J._data = J.JobExit exitCode,
            J._jobType = jobType
          }

      return exitCode

    terminateStreamingProcess streamingProcessHandle = do
      interruptProcess $ CP.streamingProcessHandleRaw streamingProcessHandle
      return $ ExitFailure 1

-- | On *nix, sends SIGINT (same as Ctrl+C) to the given process and all its descendants.
-- We don't rely on the process to stop its descendants: e.g. npm runs scripts with `sh -c`, and
-- some shells (like dash on Ubuntu) stay around as the parent of the script's command and don't
-- forward the signal to it.
-- We don't signal the whole process group either: the processes we start share it with Wasp, so
-- that would also interrupt Wasp itself, its other jobs, and whatever process started Wasp.
-- Windows has no signals, so there we terminate the process instead.
interruptProcess :: P.ProcessHandle -> IO ()
#if defined(mingw32_HOST_OS)
interruptProcess = P.terminateProcess
#else
interruptProcess processHandle =
  P.getPid processHandle >>= mapM_ interruptProcessTree

interruptProcessTree :: P.Pid -> IO ()
interruptProcessTree rootPid = do
  descendantPids <- getDescendantPids rootPid
  -- Some of the processes might have exited in the meantime, so we ignore errors.
  mapM_ (tryIOError . Signals.signalProcess Signals.sigINT) (rootPid : descendantPids)

-- | Lists the descendants of the given process with `ps`, or returns none if `ps` fails.
getDescendantPids :: P.Pid -> IO [P.Pid]
getDescendantPids rootPid =
  maybe [] (descendantsOf rootPid) <$> listPidsWithParents
  where
    -- BusyBox's `ps` doesn't support `-A`, but lists all processes without it.
    listPidsWithParents =
      runPs ["-A", "-o", "pid=", "-o", "ppid="]
        >>= maybe (runPs ["-o", "pid=", "-o", "ppid="]) (return . Just)

    runPs args =
      tryIOError (P.readProcessWithExitCode "ps" args "") >>= \case
        Right (ExitSuccess, psOutput, _) -> return $ Just $ mapMaybe parsePsLine $ lines psOutput
        _ -> return Nothing

    parsePsLine line = case mapM readMaybe (words line) of
      Just [pid, parentPid] -> Just (fromInteger pid, fromInteger parentPid)
      _ -> Nothing

    descendantsOf pid pidsWithParents =
      concatMap
        (\childPid -> childPid : descendantsOf childPid pidsWithParents)
        [childPid | (childPid, parentPid) <- pidsWithParents, parentPid == pid]
#endif

runNodeCommandAsJob :: Path' Abs (Dir a) -> String -> [String] -> J.JobType -> J.Job
runNodeCommandAsJob = runNodeCommandAsJobWithExtraEnv []

runNodeCommandAsJobWithExtraEnv :: [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> J.JobType -> J.Job
runNodeCommandAsJobWithExtraEnv = runNodeCommandAsJobWithExtraEnvAndStdin CP.Inherited

runNodeCommandAsJobWithExtraEnvAndStdin :: (CP.InputSource stdin) => stdin -> [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> J.JobType -> J.Job
runNodeCommandAsJobWithExtraEnvAndStdin stdin extraEnvVars fromDir command args jobType chan = do
  envVars <- getAllEnvVars
  let nodeCommandProcess = (P.proc command args) {P.env = Just envVars, P.cwd = Just $ SP.fromAbsDir fromDir}
  runProcessAsJobWithStdin stdin nodeCommandProcess jobType chan
  where
    -- Haskell will use the first value for variable name it finds. Since env
    -- vars in 'extraEnvVars' should override the inherited env vars, we
    -- must prepend them.
    getAllEnvVars = (extraEnvVars ++) <$> getEnvironment
