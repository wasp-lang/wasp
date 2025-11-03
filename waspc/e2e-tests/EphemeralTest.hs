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
import System.Exit (ExitCode (..))
import System.Process (callCommand, readCreateProcessWithExitCode, shell)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, expectationFailure, it, testSpec, runIO)
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

-- | Runs ephemeral test cases sequentially while building the test spec.
-- Short-circuits when any of the tests cases fails.
createAndExecuteEphemeralTestCasesSequentially ::  Path' Abs (Dir EphemeralDir) -> [EphemeralTestCase] -> Spec
createAndExecuteEphemeralTestCasesSequentially _ [] = return ()
createAndExecuteEphemeralTestCasesSequentially ephemeralDir (currentTestCase : restOfTestCases) = do
  (exitCode, _stdOut, stdErr) <- runIO $ executeEphemeralTestCase ephemeralDir currentTestCase
  it (_ephemeralTestCaseName currentTestCase) $ do
    case exitCode of
      ExitFailure _ -> expectationFailure stdErr
      ExitSuccess -> return ()
  case exitCode of
    ExitSuccess -> createAndExecuteEphemeralTestCasesSequentially ephemeralDir restOfTestCases
    ExitFailure _ -> return ()

executeEphemeralTestCase :: Path' Abs (Dir EphemeralDir) -> EphemeralTestCase -> IO (ExitCode, String, String)
executeEphemeralTestCase ephemeralDir ephemeralTestCase = do
  putStrLn $ "Executing test case: " ++ ephemeralTestCaseCommand
  readCreateProcessWithExitCode (shell ("cd " ++ fromAbsDir ephemeralDir ~&& ephemeralTestCaseCommand)) ""
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