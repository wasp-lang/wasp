module ContainerTest
  ( ContainerTest,
    makeContainerTest,
    runContainerTest,
  )
where

import ContainerTest.FileSystem (ContainerDockerfileFile, getContainerDockerfileFile)
import ContainerTest.ShellCommands (ContainerTestContext (..))
import Control.Monad (when)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    buildShellCommand,
    (~&&),
  )
import StrongPath (Abs, File, Path', fromAbsFile)
import System.Exit (ExitCode (..))
import System.Process (system)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, testSpec)

data ContainerTest = ContainerTest
  { _containerTestName :: String,
    _containerTestCommandsBuilder :: ShellCommandBuilder ContainerTestContext [ShellCommand]
  }

makeContainerTest :: String -> [ShellCommandBuilder ContainerTestContext ShellCommand] -> ContainerTest
makeContainerTest containerTestName containerTestCommandBuilders =
  ContainerTest
    { _containerTestName = containerTestName,
      _containerTestCommandsBuilder = sequence containerTestCommandBuilders
    }

-- | Runs a container test by executing container test's shell commands
-- inside of a containr.
runContainerTest :: ContainerTest -> IO TestTree
runContainerTest containerTest = do
  getContainerDockerfileFile >>= executeContainerTestWorkflow
  where
    executeContainerTestWorkflow :: Path' Abs (File ContainerDockerfileFile) -> IO TestTree
    executeContainerTestWorkflow containerDockerfileFile = do
      testSpec "Container Test" $
        describe containerTestName $ do
          it "executes successfully" $ do
            putStrLn $ "Executing container test: " ++ containerTestName

            putStrLn $ "Building docker image: " ++ dockerBuildCommand
            system dockerBuildCommand >>= failIfNotSuccess "Docker build failed"

            putStrLn $ "Starting docker container and executing the test command: " ++ dockerRunCommand
            system dockerRunCommand >>= failIfNotSuccess "Container test command failed"
      where
        containerTestName = _containerTestName containerTest

        containerTestCommand = foldr1 (~&&) $ buildShellCommand ContainerTestContext (_containerTestCommandsBuilder containerTest)
        dockerBuildCommand = "docker build --build-arg WASP_CLI_PATH=\"$(cabal list-bin wasp-cli | sed \"s|^$(pwd)/||\")\" -f " ++ fromAbsFile containerDockerfileFile ++ " -t container-test-image ."
        dockerRunCommand = "docker run --rm container-test-image bash -c '" ++ containerTestCommand ++ "'"

        failIfNotSuccess errorMessage exitCode = when (exitCode /= ExitSuccess) $ error errorMessage
