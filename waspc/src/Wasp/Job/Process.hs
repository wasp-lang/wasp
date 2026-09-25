module Wasp.Job.Process (Subprocess, runChecked, runReturningExitCode, spawn, wait, poll, stop) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ReleaseKey, allocate, release)
import qualified Data.Text as T
import System.Exit (ExitCode)
import qualified System.Process as P
import Wasp.Job (Job, JobOutputSink, getJobOutputSink, requireExitSuccess)
import qualified Wasp.Job.Output.Event as Event
import qualified Wasp.Process as Process
import qualified Wasp.Process.Managed as Managed

-- | Fails the job on a nonzero child exit.
runChecked :: Process.InputMode -> P.CreateProcess -> Job ()
runChecked inputMode process = runReturningExitCode inputMode process >>= requireExitSuccess

runReturningExitCode :: Process.InputMode -> P.CreateProcess -> Job ExitCode
runReturningExitCode inputMode process = do
  emit <- getJobOutputSink
  liftIO $ Process.run inputMode process (forwardProcessOutput emit)

forwardProcessOutput :: JobOutputSink -> Process.OutputStream -> T.Text -> IO ()
forwardProcessOutput emit stream = emit $ case stream of
  Process.Stdout -> Event.Stdout
  Process.Stderr -> Event.Stderr

data Subprocess = Subprocess ReleaseKey Managed.ManagedSubprocess

spawn :: P.CreateProcess -> Job Subprocess
spawn process = do
  emit <- getJobOutputSink
  (releaseKey, subprocess) <- allocate (Managed.start process $ forwardProcessOutput emit) Managed.stop
  return $ Subprocess releaseKey subprocess

wait :: Subprocess -> IO ExitCode
wait (Subprocess _ subprocess) = Managed.waitForRootExit subprocess

poll :: Subprocess -> IO (Maybe ExitCode)
poll (Subprocess _ subprocess) = Managed.pollRootExit subprocess

stop :: Subprocess -> Job ()
stop (Subprocess releaseKey _) = release releaseKey
