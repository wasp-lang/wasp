module Wasp.Job.Node
  ( makeCreateProcess,
    makeCreateProcessWithExtraEnv,
    run,
    runReturningExitCode,
    makeJob,
    makeJobWithExtraEnv,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Environment (getEnvironment)
import System.Exit (ExitCode)
import qualified System.Process as P
import qualified Wasp.Job as Job
import Wasp.Job.Internal (failWithExitCode)
import qualified Wasp.Job.Subprocess as Subprocess
import qualified Wasp.Node.Version as NodeVersion

makeJob :: Path' Abs (Dir a) -> String -> [String] -> Job.JobKind -> Job.Job
makeJob = makeJobWithExtraEnv []

makeJobWithExtraEnv :: [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> Job.JobKind -> Job.Job
makeJobWithExtraEnv extraEnvVars workingDir executable arguments jobKind =
  Job.makeJob jobKind $
    runCommandUsing Subprocess.run extraEnvVars workingDir executable arguments

-- | Runs the command to completion, failing the Job on a nonzero child exit.
run :: Path' Abs (Dir a) -> String -> [String] -> Job.JobAction ()
run = runCommandUsing Subprocess.run []

-- | Runs the command and returns the child process's exit status for explicit handling.
runReturningExitCode :: Path' Abs (Dir a) -> String -> [String] -> Job.JobAction ExitCode
runReturningExitCode = runCommandUsing Subprocess.runReturningExitCode []

runCommandUsing :: (P.CreateProcess -> Job.JobAction a) -> [(String, String)] -> Path' Abs (Dir dir) -> String -> [String] -> Job.JobAction a
runCommandUsing runProcess extraEnvVars workingDir executable arguments = do
  requireValidNodeAndNpm
  process <- liftIO $ makeCreateProcessWithExtraEnv extraEnvVars workingDir executable arguments
  runProcess process

requireValidNodeAndNpm :: Job.JobAction ()
requireValidNodeAndNpm =
  liftIO NodeVersion.checkUserNodeAndNpmMeetWaspRequirements >>= \case
    NodeVersion.VersionCheckFail errorMsg -> do
      Job.emitJobOutput Job.Stderr $ T.pack errorMsg
      failWithExitCode 1
    NodeVersion.VersionCheckSuccess -> return ()

makeCreateProcess :: Path' Abs (Dir a) -> String -> [String] -> IO P.CreateProcess
makeCreateProcess = makeCreateProcessWithExtraEnv []

makeCreateProcessWithExtraEnv :: [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> IO P.CreateProcess
makeCreateProcessWithExtraEnv extraEnvVars workingDir executable arguments = do
  envVars <- getAllEnvVars
  return $ (P.proc executable arguments) {P.env = Just envVars, P.cwd = Just $ SP.fromAbsDir workingDir}
  where
    -- Haskell will use the first value for variable name it finds. Since env
    -- vars in 'extraEnvVars' should override the inherited env vars, we
    -- must prepend them.
    getAllEnvVars = (extraEnvVars ++) <$> getEnvironment
