module Test
  ( Test,
    TestCase,
    makeTest,
    makeTestCase,
    runTest,
  )
where

import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    buildShellCommand,
  )
import StrongPath (Abs, Dir, Path', fromAbsDir, (</>))
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (..), StdStream (..), callCommand, createProcess, shell, waitForProcess)
import Test.FileSystem (TestDir, asWaspProjectDir, getE2eDir, getTestsTempDir, testWaspProjectDirInTestDir)
import Test.Hspec (Spec, expectationFailure, it, runIO)
import Test.ShellCommands (E2eTestContext (..))
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import WaspProject.ShellCommands (WaspProjectContext (..))

data Test = Test
  { _testName :: String,
    _testCases :: [TestCase]
  }

makeTest :: String -> [TestCase] -> Test
makeTest testName testCases =
  Test
    { _testName = testName,
      _testCases = testCases
    }

-- | Represent a single test case of some 'Test'.
data TestCase = TestCase
  { _testCaseName :: String,
    _testCaseCommandBuilder :: ShellCommandBuilder E2eTestContext ShellCommand
  }

makeTestCase :: String -> ShellCommandBuilder E2eTestContext ShellCommand -> TestCase
makeTestCase testCaseName commandBuilder =
  TestCase
    { _testCaseName = testCaseName,
      _testCaseCommandBuilder = commandBuilder
    }

-- | Runs a 'Test' by executing all e2e test cases' shell commands and then checking their exit code.
-- It executes the shell commands while builing the 'Spec'. This is to enforce the sequential execution of test cases.
runTest :: Test -> IO TestTree
runTest test = do
  e2eDir <- getE2eDir (_testName test)

  setupTest e2eDir
  createAndExecuteTest e2eDir test

setupTest :: Path' Abs (Dir TestDir) -> IO ()
setupTest e2eDir = do
  e2eTestsTempDir <- getTestsTempDir

  callCommand $ "mkdir -p " ++ fromAbsDir e2eTestsTempDir
  callCommand $ "rm -rf " ++ fromAbsDir e2eDir
  callCommand $ "mkdir " ++ fromAbsDir e2eDir

createAndExecuteTest :: Path' Abs (Dir TestDir) -> Test -> IO TestTree
createAndExecuteTest e2eDir test = do
  putStrLn $ "Creating e2e test: " ++ _testName test
  testSpec
    (_testName test)
    (mapM_ (createAndExecuteTestCase e2eDir) (_testCases test))

createAndExecuteTestCase :: Path' Abs (Dir TestDir) -> TestCase -> Spec
createAndExecuteTestCase e2eDir e2eTestCase = do
  runIO $ putStrLn $ "Executing test case: " ++ _testCaseName e2eTestCase
  runIO $ putStrLn $ "Command: " ++ e2eTestCaseCommand
  (_, _, _, processHandle) <-
    runIO $
      createProcess
        (shell e2eTestCaseCommand)
          { cwd = Just $ fromAbsDir e2eDir,
            std_in = Inherit,
            std_out = Inherit,
            std_err = Inherit
          }
  exitCode <- runIO $ waitForProcess processHandle

  it (_testCaseName e2eTestCase) $ do
    case exitCode of
      ExitFailure _ -> expectationFailure $ "Command failed: " ++ e2eTestCaseCommand
      ExitSuccess -> return ()
  where
    e2eTestCaseCommand :: ShellCommand
    e2eTestCaseCommand = buildShellCommand e2eTestContext (_testCaseCommandBuilder e2eTestCase)

    e2eTestContext :: E2eTestContext
    e2eTestContext =
      E2eTestContext
        { _e2eDir = e2eDir,
          _e2eWaspProjectContext =
            WaspProjectContext
              { _waspProjectDir = asWaspProjectDir $ e2eDir </> testWaspProjectDirInTestDir "wasp-app",
                _waspProjectName = "wasp-app"
              }
        }
