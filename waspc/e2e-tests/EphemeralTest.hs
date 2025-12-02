module EphemeralTest
  ( EphemeralTest,
    EphemeralTestCase,
    makeEphemeralTest,
    makeEphemeralTestCase,
    runEphemeralTest,
  )
where

import EphemeralTest.FileSystem (EphemeralDir, asWaspProjectDir, ephemeralWaspProjectDirInEphemeralDir, getEphemeralDir)
import EphemeralTest.ShellCommands (EphemeralTestContext (..))
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    buildShellCommand,
  )
import StrongPath (Abs, Dir, Path', fromAbsDir, (</>))
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (..), StdStream (..), callCommand, createProcess, shell, waitForProcess)
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, sequential)
import Test.Hspec.Runner (Config (..), defaultConfig)
import Test.Tasty (DependencyType (..), TestTree, sequentialTestGroup)
import Test.Tasty.Hspec (testSpec)
import WaspProject.ShellCommands (WaspProjectContext (..))

data EphemeralTest = EphemeralTest
  { _ephemeralTestName :: String,
    _ephemeralTestCases :: [EphemeralTestCase]
  }

makeEphemeralTest :: String -> [EphemeralTestCase] -> EphemeralTest
makeEphemeralTest ephemeralTestName ephemeralTestCases =
  EphemeralTest
    { _ephemeralTestName = ephemeralTestName,
      _ephemeralTestCases = ephemeralTestCases
    }

-- | Represent a single test case of some 'EpherealTest'.
data EphemeralTestCase = EphemeralTestCase
  { _ephemeralTestCaseName :: String,
    _ephemeralTestCaseCommandBuilder :: ShellCommandBuilder EphemeralTestContext ShellCommand
  }

makeEphemeralTestCase :: String -> ShellCommandBuilder EphemeralTestContext ShellCommand -> EphemeralTestCase
makeEphemeralTestCase testCaseName commandBuilder =
  EphemeralTestCase
    { _ephemeralTestCaseName = testCaseName,
      _ephemeralTestCaseCommandBuilder = commandBuilder
    }

-- | Runs a 'EphemeralTest' by executing all ephemeral test cases' shell commands and then checking their exit code.
-- It executes the shell commands while builing the 'Spec'. This is to enforce the sequential execution of test cases.
runEphemeralTest :: EphemeralTest -> IO TestTree
runEphemeralTest ephemeralTest = do
  ephemeralDir <- getEphemeralDir (_ephemeralTestName ephemeralTest)

  setupEphemeralTest ephemeralDir
  createAndExecuteEphemeralTest ephemeralDir ephemeralTest

setupEphemeralTest :: Path' Abs (Dir EphemeralDir) -> IO ()
setupEphemeralTest ephemeralDir = do
  callCommand $ "rm -rf " ++ fromAbsDir ephemeralDir
  callCommand $ "mkdir " ++ fromAbsDir ephemeralDir

createAndExecuteEphemeralTest :: Path' Abs (Dir EphemeralDir) -> EphemeralTest -> IO TestTree
createAndExecuteEphemeralTest ephemeralDir ephemeralTest = do
  putStrLn $ "Creating ephemeral test: " ++ _ephemeralTestName ephemeralTest
  testSpec
    (_ephemeralTestName ephemeralTest)
    (mapM_ (createAndExecuteEphemeralTestCase ephemeralDir) (_ephemeralTestCases ephemeralTest))

createAndExecuteEphemeralTestCase :: Path' Abs (Dir EphemeralDir) -> EphemeralTestCase -> Spec
createAndExecuteEphemeralTestCase ephemeralDir ephemeralTestCase = do
  runIO $ putStrLn $ "Executing test case: " ++ _ephemeralTestCaseName ephemeralTestCase
  runIO $ putStrLn $ "Command: " ++ ephemeralTestCaseCommand
  (_, _, _, processHandle) <-
    runIO $
      createProcess
        (shell ephemeralTestCaseCommand)
          { cwd = Just $ fromAbsDir ephemeralDir,
            std_in = Inherit,
            std_out = Inherit,
            std_err = Inherit
          }
  exitCode <- runIO $ waitForProcess processHandle

  it (_ephemeralTestCaseName ephemeralTestCase) $ do
    case exitCode of
      ExitFailure _ -> expectationFailure $ "Command failed: " ++ ephemeralTestCaseCommand
      ExitSuccess -> return ()
  where
    ephemeralTestCaseCommand :: ShellCommand
    ephemeralTestCaseCommand = buildShellCommand ephemeralTestContext (_ephemeralTestCaseCommandBuilder ephemeralTestCase)

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
