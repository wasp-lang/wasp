{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Test
  ( Test (..),
    TestCase (..),
    runTests,
  )
where

import Control.Concurrent.Async (mapConcurrently)
import Data.Maybe (fromJust)
import FileSystem (TestCaseDir, getTestCaseDir, getTestOutputsDir)
import ShellCommands (ShellCommand, ShellCommandBuilder, TestContext (..), WaspProjectContext (..), buildShellCommand, (~&&))
import StrongPath (Abs, Dir, Path', fromAbsDir, parseRelDir, (</>))
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (..), StdStream (..), callCommand, createProcess, shell, waitForProcess)
import Test.Hspec (Spec, expectationFailure, it)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

data Test = Test
  { name :: String,
    testCases :: [TestCase]
  }

-- | Represent a single test case of some 'Test'.
data TestCase = TestCase
  { name :: String,
    shellCommandBuilder :: ShellCommandBuilder TestContext [ShellCommand]
  }

-- | Prepares a list of 'Test's in parallel (executing shell commands and capturing exit codes),
--  and then creates test trees that check the results.
runTests :: [Test] -> IO [TestTree]
runTests tests = do
  testOutputsDir <- getTestOutputsDir
  callCommand $ "mkdir -p " ++ fromAbsDir testOutputsDir

  mapConcurrently prepareTest tests >>= mapM createTestTree

-- | Data needed to create a test tree.
data TestData = TestData
  { name :: String,
    testCaseResults :: [TestCaseResult]
  }

-- | Result of executing a single test case.
data TestCaseResult = TestCaseResult
  { name :: String,
    command :: String,
    exitCode :: ExitCode
  }

-- | Prepares a test by executing all its test cases sequentially and capturing results.
prepareTest :: Test -> IO TestData
prepareTest test = do
  testCaseResults <- mapM (executeTestCase test.name) test.testCases
  return TestData {name = test.name, testCaseResults}

createTestTree :: TestData -> IO TestTree
createTestTree testData =
  testSpec testData.name (mapM_ checkTestCaseResult testData.testCaseResults)
  where
    checkTestCaseResult :: TestCaseResult -> Spec
    checkTestCaseResult result =
      it result.name $ do
        case result.exitCode of
          ExitFailure _ -> expectationFailure $ "Command failed: " ++ result.command
          ExitSuccess -> return ()

-- | Executes a single test case and captures its result.
executeTestCase :: String -> TestCase -> IO TestCaseResult
executeTestCase testName testCase = do
  testCaseDir <- getTestCaseDir testName testCase.name
  let waspProjectContext =
        WaspProjectContext
          { waspProjectDir = testCaseDir </> (fromJust . parseRelDir $ "wasp-app"),
            waspProjectName = "wasp-app"
          }
      testContext = TestContext {testCaseDir, waspProjectContext}
      testCaseCommand = foldr1 (~&&) $ buildShellCommand testContext testCase.shellCommandBuilder

  setupTestCase testCaseDir
  putStrLn $ "Executing test case: " ++ testName ++ "/" ++ testCase.name
  putStrLn $ "Command: " ++ testCaseCommand
  (_, _, _, processHandle) <-
    createProcess
      (shell testCaseCommand)
        { cwd = Just $ fromAbsDir testCaseDir,
          std_in = Inherit,
          std_out = Inherit,
          std_err = Inherit
        }
  exitCode <- waitForProcess processHandle

  return TestCaseResult {name = testCase.name, command = testCaseCommand, exitCode}

setupTestCase :: Path' Abs (Dir TestCaseDir) -> IO ()
setupTestCase testCaseDir = do
  callCommand $ "rm -rf " ++ fromAbsDir testCaseDir
  callCommand $ "mkdir -p " ++ fromAbsDir testCaseDir
