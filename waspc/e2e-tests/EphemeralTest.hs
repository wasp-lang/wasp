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
    (~&&),
  )
import StrongPath (Abs, Dir, Path', fromAbsDir, (</>))
import System.Directory.Internal.Prelude (unless)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (..), StdStream (..), callCommand, createProcess, readCreateProcessWithExitCode, shell, waitForProcess)
import Test.Hspec (Spec, expectationFailure, it, runIO, sequential)
import Test.Tasty (TestTree)
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
-- It executes the commands while builing the 'Spec', because we force sequential execution of ephemeral test cases.
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
    (createAndExecuteEphemeralTestCasesSequentially ephemeralDir (_ephemeralTestCases ephemeralTest))

createAndExecuteEphemeralTestCasesSequentially :: Path' Abs (Dir EphemeralDir) -> [EphemeralTestCase] -> Spec
createAndExecuteEphemeralTestCasesSequentially ephemeralDir ephemeralTestCases =
  sequential $ mapM_ (createAndExecuteEphemeralTestCase ephemeralDir) ephemeralTestCases

createAndExecuteEphemeralTestCase :: Path' Abs (Dir EphemeralDir) -> EphemeralTestCase -> Spec
createAndExecuteEphemeralTestCase ephemeralDir ephemeralTestCase = do
  runIO $ putStrLn $ "Executing test case: " ++ ephemeralTestCaseName
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

  runIO $ putStrLn $ "Finished with exit code: " ++ show exitCode

  it (_ephemeralTestCaseName ephemeralTestCase) $ do
    case exitCode of
      ExitFailure _ -> expectationFailure $ "Command failed: " ++ ephemeralTestCaseCommand
      ExitSuccess -> return ()
  where
    ephemeralTestCaseName :: String
    ephemeralTestCaseName = _ephemeralTestCaseName ephemeralTestCase

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
