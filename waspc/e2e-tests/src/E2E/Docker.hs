-- | Tests and steps that need Docker, which a run can disable (see
-- 'E2E.SuiteEnv.skipDockerEnvVar').
module E2E.Docker
  ( dockerTest,
    whenDockerEnabled,
    docker,
    dockerCommand,
    dockerCleanup,
  )
where

import Control.Monad (void)
import Control.Monad.Reader (asks)
import E2E.Command (Command (..), cleanupCommandTimeoutSeconds, program)
import E2E.Process (ProcessResult, run, runAllowingFailure)
import E2E.SuiteEnv (TestSuiteEnv (..), skipDockerEnvVar)
import E2E.TestM (TestEnv (..), TestM, logLine)
import E2E.TestTree (TestTree, skippedTest, test, withTestSuiteEnv)

-- | A test that needs Docker. When Docker is disabled, it passes without
-- running any of its steps.
--
-- Not for snapshot tests, which must produce a snapshot: use
-- 'whenDockerEnabled' in their steps.
dockerTest :: String -> TestM a -> TestTree
dockerTest name steps =
  withTestSuiteEnv $ \testSuiteEnv ->
    if testSuiteEnv.isDockerEnabled
      then test name steps
      else skippedTest name dockerDisabledMessage

-- | For steps that only some part of a test needs Docker for.
whenDockerEnabled :: TestM () -> TestM ()
whenDockerEnabled steps = do
  isDockerEnabled <- asks (.testSuiteEnv.isDockerEnabled)
  if isDockerEnabled then steps else logLine dockerDisabledMessage

dockerDisabledMessage :: String
dockerDisabledMessage = "SKIPPED: Docker is disabled via the " ++ skipDockerEnvVar ++ " environment variable."

-- | Runs a Docker CLI command and fails the test if it doesn't succeed.
docker :: [String] -> TestM ProcessResult
docker = run . dockerCommand

-- | For Docker CLI commands that need different settings, e.g. a shorter
-- timeout.
dockerCommand :: [String] -> Command
dockerCommand = program "docker"

-- | For cleanup steps: the resource may already be gone, and nothing can cut
-- a hung cleanup short.
dockerCleanup :: [String] -> TestM ()
dockerCleanup arguments =
  void $ runAllowingFailure (dockerCommand arguments) {timeoutSeconds = cleanupCommandTimeoutSeconds}
