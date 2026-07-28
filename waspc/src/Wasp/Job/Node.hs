module Wasp.Job.Node
  ( makeCreateProcess,
    makeCreateProcessWithExtraEnv,
    run,
    runWithExtraEnv,
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
makeJobWithExtraEnv extraEnvVars fromDir command args jobKind =
  Job.makeJob jobKind $
    runWithExtraEnv extraEnvVars fromDir command args

-- | Runs the command to completion, failing the Job on a nonzero child exit.
run :: Path' Abs (Dir a) -> String -> [String] -> Job.JobAction ()
run = runWithExtraEnv []

runWithExtraEnv :: [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> Job.JobAction ()
runWithExtraEnv extraEnvVars fromDir command args =
  runWithExtraEnvReturningExitCode extraEnvVars fromDir command args >>= Job.requireExitSuccess

-- | Runs the command and returns the child process's exit status for explicit handling.
runReturningExitCode :: Path' Abs (Dir a) -> String -> [String] -> Job.JobAction ExitCode
runReturningExitCode = runWithExtraEnvReturningExitCode []

runWithExtraEnvReturningExitCode :: [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> Job.JobAction ExitCode
runWithExtraEnvReturningExitCode extraEnvVars fromDir command args = do
  requireValidNodeAndNpm
  nodeCommandProcess <- liftIO $ makeCreateProcessWithExtraEnv extraEnvVars fromDir command args
  Subprocess.runReturningExitCode nodeCommandProcess

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
makeCreateProcessWithExtraEnv extraEnvVars fromDir command args = do
  envVars <- getAllEnvVars
  return $ (P.proc command args) {P.env = Just envVars, P.cwd = Just $ SP.fromAbsDir fromDir}
  where
    -- Haskell will use the first value for variable name it finds. Since env
    -- vars in 'extraEnvVars' should override the inherited env vars, we
    -- must prepend them.
    getAllEnvVars = (extraEnvVars ++) <$> getEnvironment
