{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Test
  ( Test,
    TestCase,
    makeTest,
    makeTestCase,
    runTest,
  )
where

import Data.Maybe (fromJust)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    buildShellCommand,
  )
import StrongPath (Abs, Dir, Path', fromAbsDir, parseRelDir, (</>))
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (..), StdStream (..), callCommand, createProcess, shell, waitForProcess)
import Test.FileSystem (TestDir, getTestDir, getTestOutputsDir)
import Test.Hspec (Spec, expectationFailure, it, runIO)
import Test.ShellCommands (TestContext (..))
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import WaspProject.ShellCommands (WaspProjectContext (..))

data Test = Test
  { name :: String,
    testCases :: [TestCase]
  }

makeTest :: String -> [TestCase] -> Test
makeTest testName testTestCases =
  Test
    { name = testName,
      testCases = testTestCases
    }

-- | Represent a single test case of some 'Test'.
data TestCase = TestCase
  { name :: String,
    commandBuilder :: ShellCommandBuilder TestContext ShellCommand
  }

makeTestCase :: String -> ShellCommandBuilder TestContext ShellCommand -> TestCase
makeTestCase testCaseName testCaseCommandBuilder =
  TestCase
    { name = testCaseName,
      commandBuilder = testCaseCommandBuilder
    }

-- | Runs a 'Test' by executing all test cases' shell commands and then checking their exit code.
-- It executes the shell commands while builing the 'Spec'. This is to enforce the sequential execution of test cases.
runTest :: Test -> IO TestTree
runTest test = do
  testDir <- getTestDir test.name

  setupTest testDir
  createAndExecuteTest testDir test

setupTest :: Path' Abs (Dir TestDir) -> IO ()
setupTest testDir = do
  testOutputsDir <- getTestOutputsDir

  callCommand $ "mkdir -p " ++ fromAbsDir testOutputsDir
  callCommand $ "rm -rf " ++ fromAbsDir testDir
  callCommand $ "mkdir " ++ fromAbsDir testDir

createAndExecuteTest :: Path' Abs (Dir TestDir) -> Test -> IO TestTree
createAndExecuteTest testDir test = do
  putStrLn $ "Creating test: " ++ test.name
  testSpec
    test.name
    (mapM_ (createAndExecuteTestCase testDir) test.testCases)

createAndExecuteTestCase :: Path' Abs (Dir TestDir) -> TestCase -> Spec
createAndExecuteTestCase testDir testCase = do
  runIO $ putStrLn $ "Executing test case: " ++ testCase.name
  runIO $ putStrLn $ "Command: " ++ testCaseCommand
  (_, _, _, processHandle) <-
    runIO $
      createProcess
        (shell testCaseCommand)
          { cwd = Just $ fromAbsDir testDir,
            std_in = Inherit,
            std_out = Inherit,
            std_err = Inherit
          }
  exitCode <- runIO $ waitForProcess processHandle

  it testCase.name $ do
    case exitCode of
      ExitFailure _ -> expectationFailure $ "Command failed: " ++ testCaseCommand
      ExitSuccess -> return ()
  where
    testCaseCommand :: ShellCommand
    testCaseCommand = buildShellCommand testContext testCase.commandBuilder

    testContext :: TestContext
    testContext =
      TestContext
        { testDir,
          waspProjectContext
        }

    waspProjectContext :: WaspProjectContext
    waspProjectContext =
      WaspProjectContext
        { _waspProjectDir = testDir </> (fromJust . parseRelDir $ "wasp-app"),
          _waspProjectName = "wasp-app"
        }
