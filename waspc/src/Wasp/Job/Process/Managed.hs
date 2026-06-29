module Wasp.Job.Process.Managed
  ( configureCreateProcess,
    stopProcessTree,
  )
where

import qualified System.Info
import qualified System.Process as P
import System.Timeout (timeout)

-- | Configure a child process whose lifecycle is owned by Wasp while Wasp keeps running.
configureCreateProcess :: P.CreateProcess -> P.CreateProcess
configureCreateProcess process =
  if supportsProcessGroups
    then process {P.create_group = True}
    else process

-- | Stop a process and its subprocesses, waiting until the OS releases its resources.
stopProcessTree :: P.ProcessHandle -> IO ()
stopProcessTree processHandle = do
  processExitCode <- P.getProcessExitCode processHandle
  case processExitCode of
    Just _ -> return ()
    Nothing -> do
      signalProcessTree processHandle
      waitForProcessToExit processHandle

signalProcessTree :: P.ProcessHandle -> IO ()
signalProcessTree processHandle =
  if supportsProcessGroups
    then P.interruptProcessGroupOf processHandle
    else P.terminateProcess processHandle

waitForProcessToExit :: P.ProcessHandle -> IO ()
waitForProcessToExit processHandle = do
  exitCodeAfterInterrupt <- timeout processStopTimeout $ P.waitForProcess processHandle
  case exitCodeAfterInterrupt of
    Just _ -> return ()
    Nothing -> do
      P.terminateProcess processHandle
      _ <- P.waitForProcess processHandle
      return ()

processStopTimeout :: Int
processStopTimeout = 5 * 1000000

supportsProcessGroups :: Bool
supportsProcessGroups =
  -- On Unix, we use process groups because many commands spawn child processes.
  -- On Windows, creating a new process group can hang if the process needs stdin.
  -- Ref: https://stackoverflow.com/questions/61856063/spawning-a-process-with-create-group-true-set-pgid-hangs-when-starting-docker
  System.Info.os /= "mingw32"
