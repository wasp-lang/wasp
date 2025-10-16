module EphemeralTest
  ( EphemeralTest,
    makeEphemeralTest,
    runEphemeralTest,
  )
where

import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    buildShellCommand,
    (~&&),
  )
import StrongPath (Abs, Path', Dir, fromAbsDir, (</>))
import System.Exit (ExitCode (..))
import System.Process (callCommand, readCreateProcessWithExitCode, shell)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, testSpec, expectationFailure)
import EphemeralTest.ShellCommands (EphemeralTestContext (..))
import EphemeralTest.FileSystem (EphemeralDir, getEphemeralDir, asWaspProjectDir, ephemeralWaspProjectDirInEphemeralDir)
import WaspProject.ShellCommands (WaspProjectContext(..))

data EphemeralTest = EphemeralTest
  { _ephemeralTestName :: String,
    _ephemeralTestCommandsBuilder :: ShellCommandBuilder EphemeralTestContext [ShellCommand]
  }

makeEphemeralTest :: String -> [ShellCommandBuilder EphemeralTestContext ShellCommand] -> EphemeralTest
makeEphemeralTest ephemeralTestName ephemeralTestCommandBuilders =
  EphemeralTest
    { _ephemeralTestName = ephemeralTestName,
      _ephemeralTestCommandsBuilder = sequence ephemeralTestCommandBuilders
    }

-- | Runs 
runEphemeralTest :: EphemeralTest -> IO TestTree
runEphemeralTest ephemeralTest = do
  getEphemeralDir ephemeralTestName >>= executeEphemeralTestWorkflow
  where
    ephemeralTestName :: String
    ephemeralTestName = _ephemeralTestName ephemeralTest

    executeEphemeralTestWorkflow :: Path' Abs (Dir EphemeralDir) -> IO TestTree
    executeEphemeralTestWorkflow ephemeralDir = do
      setupEphemeralTest
      executeEphemeralTestCommand
      where
        setupEphemeralTest :: IO ()
        setupEphemeralTest = do
          callCommand $ "rm -rf " ++ fromAbsDir ephemeralDir
          callCommand $ "mkdir " ++ fromAbsDir ephemeralDir

        executeEphemeralTestCommand :: IO TestTree
        executeEphemeralTestCommand = do
          putStrLn $ "Executing ephemeral test: " ++ ephemeralTestName
          putStrLn $ "Running the following command: " ++ ephemeralTestCommand
          testSpec "Ephemeral Test" $
            describe ephemeralTestName $ do
              it "executes successfully" $ do
                -- We purposely remove the empheralDir as part of the ephemeralTestCommand.
                -- Because if the test fails, we don't want to delete the test dir so we can inspect the faulty test files.
                (exitCode, _stdOut, stdErr) <- readCreateProcessWithExitCode (shell ("cd " ++ fromAbsDir ephemeralDir ~&& ephemeralTestCommand ~&& "rm -rf " ++ fromAbsDir ephemeralDir)) ""
                case exitCode of
                  ExitFailure _ -> expectationFailure stdErr
                  ExitSuccess -> return ()

        ephemeralTestCommand :: ShellCommand
        ephemeralTestCommand = foldr1 (~&&) $ buildShellCommand ephemeralTestContext (_ephemeralTestCommandsBuilder ephemeralTest)

        ephemeralTestContext :: EphemeralTestContext
        ephemeralTestContext =
          EphemeralTestContext
            { _ephemeralDir = ephemeralDir,
              _ephemeralWaspProjectContext =
                WaspProjectContext
                  { _waspProjectDir = asWaspProjectDir $ ephemeralDir </> ephemeralWaspProjectDirInEphemeralDir "wasp-app",
                    _waspProjectName = "wasp-app"
                  }
            }
