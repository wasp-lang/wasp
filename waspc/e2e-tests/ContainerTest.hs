module ContainerTest
  ( ContainerTest,
    makeContainerTest,
    runContainerTest,
  )
where

import ContainerTest.FileSystem (ContainerDockerfileFile, getContainerDockerfileFile)
import ContainerTest.ShellCommands (ContainerTestContext (..))
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    buildShellCommand,
    (~&&),
  )
import StrongPath (Abs, File, Path', fromAbsFile)
import System.Exit (ExitCode (..))
import System.Process (readCreateProcessWithExitCode, shell)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, expectationFailure, it, testSpec)

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
    containerTestName :: String
    containerTestName = _containerTestName containerTest

    executeContainerTestWorkflow :: Path' Abs (File ContainerDockerfileFile) -> IO TestTree
    executeContainerTestWorkflow containerDockerfileFile = do
      putStrLn $ "Executing container test: " ++ containerTestName
      testSpec "Container Test" $
        describe containerTestName $ do
          it "executes successfully" $ do
            (dockerBuildExitCode, _stdOut, dockerBuildStdErr) <- readCreateProcessWithExitCode (shell dockerBuildCommand) ""
            case dockerBuildExitCode of
              ExitFailure _ -> expectationFailure $ "docker build failed: " ++ dockerBuildStdErr
              ExitSuccess -> return ()
            (dockerRunExitCode, _stdOut, dockerRunStdErr) <- readCreateProcessWithExitCode (shell dockerRunCommand) ""
            case dockerRunExitCode of
              ExitFailure _ -> expectationFailure $ "docker run failed: " ++ dockerRunStdErr
              ExitSuccess -> return ()
      where
        dockerBuildCommand :: ShellCommand
        dockerBuildCommand = "docker build --build-arg WASP_CLI_PATH=\"$(" ++ waspCliFilePathRelativeToWaspcDirCommand ++ ")\" -f " ++ fromAbsFile containerDockerfileFile ++ " -t " ++ dockerContainerImageName ++ " -q ."

        waspCliFilePathRelativeToWaspcDirCommand :: ShellCommand
        waspCliFilePathRelativeToWaspcDirCommand = "cabal list-bin wasp-cli | sed \"s|^$(pwd)/||\""

        dockerRunCommand :: ShellCommand
        dockerRunCommand = "docker run --rm -i " ++ dockerContainerImageName ++ " bash -s <<'EOF'\n" ++ containerTestCommand ++ "\nEOF"
        containerTestCommand = foldr1 (~&&) $ buildShellCommand ContainerTestContext (_containerTestCommandsBuilder containerTest)

        dockerContainerImageName :: String
        dockerContainerImageName = "container-test-" ++ containerTestName
