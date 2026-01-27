{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Test
  ( Test (..),
    TestCase (..),
    runTest,
  )
where

import Data.Maybe (fromJust)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    buildShellCommand,
    (~&&),
  )
import StrongPath (Abs, Dir, Path', fromAbsDir, parseRelDir, (</>))
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (..), StdStream (..), callCommand, createProcess, shell, waitForProcess)
import Test.FileSystem (TestCaseDir, getTestCaseDir, getTestOutputsDir)
import Test.Hspec (Spec, expectationFailure, it, runIO)
import Test.ShellCommands (TestContext (..))
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import WaspProject.ShellCommands (WaspProjectContext (..))

data Test = Test
  { name :: String,
    testCases :: [TestCase]
  }

-- | Represent a single test case of some 'Test'.
data TestCase = TestCase
  { name :: String,
    shellCommandBuilder :: ShellCommandBuilder TestContext [ShellCommand]
  }

-- | Runs a 'Test' by executing all test cases' shell commands and then checking their exit code.
-- Each test case runs in its own isolated directory.
runTest :: Test -> IO TestTree
runTest test = do
  testOutputsDir <- getTestOutputsDir
  callCommand $ "mkdir -p " ++ fromAbsDir testOutputsDir

  testSpec test.name (mapM_ (createAndExecuteTestCase test.name) test.testCases)

createAndExecuteTestCase :: String -> TestCase -> Spec
createAndExecuteTestCase testName testCase = do
  testCaseDir <- runIO $ getTestCaseDir testName testCase.name
  runIO $ setupTestCase testCaseDir

  runIO $ putStrLn $ "Executing test case: " ++ testName ++ "/" ++ testCase.name
  runIO $ putStrLn $ "Command: " ++ testCaseCommand testCaseDir
  (_, _, _, processHandle) <-
    runIO $
      createProcess
        (shell $ testCaseCommand testCaseDir)
          { cwd = Just $ fromAbsDir testCaseDir,
            std_in = Inherit,
            std_out = Inherit,
            std_err = Inherit
          }
  exitCode <- runIO $ waitForProcess processHandle

  it testCase.name $ do
    case exitCode of
      ExitFailure _ -> expectationFailure $ "Command failed: " ++ testCaseCommand testCaseDir
      ExitSuccess -> return ()
  where
    testCaseCommand :: Path' Abs (Dir TestCaseDir) -> ShellCommand
    testCaseCommand testCaseDir = foldr1 (~&&) $ buildShellCommand (testContext testCaseDir) testCase.shellCommandBuilder

    testContext :: Path' Abs (Dir TestCaseDir) -> TestContext
    testContext testCaseDir =
      TestContext
        { testCaseDir = testCaseDir,
          waspProjectContext = waspProjectContext testCaseDir
        }

    waspProjectContext :: Path' Abs (Dir TestCaseDir) -> WaspProjectContext
    waspProjectContext testCaseDir =
      WaspProjectContext
        { _waspProjectDir = testCaseDir </> (fromJust . parseRelDir $ "wasp-app"),
          _waspProjectName = "wasp-app"
        }

setupTestCase :: Path' Abs (Dir TestCaseDir) -> IO ()
setupTestCase testCaseDir = do
  callCommand $ "rm -rf " ++ fromAbsDir testCaseDir
  callCommand $ "mkdir -p " ++ fromAbsDir testCaseDir
