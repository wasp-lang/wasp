{-# LANGUAGE DuplicateRecordFields #-}

module Test
  ( Test (..),
    TestCase (..),
    testTreeFromTest,
  )
where

import Control.Monad (unless)
import Data.Maybe (fromJust)
import FileSystem (TestCaseDir, TestLogFile, getTestCaseDir, testCaseLogFileInTestCaseDir)
import ShellCommands (ShellCommand, ShellCommandBuilder, TestContext (..), WaspProjectContext (..), buildShellCommand, (~&&))
import StrongPath (Abs, Dir, File, Path', fromAbsDir, parseRelDir, (</>))
import System.Directory (removeDirectoryRecursive)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (..), StdStream (..), callCommand, createProcess, shell, waitForProcess)
import Test.Hspec (Spec, expectationFailure, it)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import TestLogging (formatCommandFailure, openLogForCommand)

data Test = Test
  { name :: String,
    testCases :: [TestCase]
  }

-- | Represent a single test case of some 'Test'.
data TestCase = TestCase
  { name :: String,
    shellCommandBuilder :: ShellCommandBuilder TestContext [ShellCommand]
  }

testTreeFromTest :: Test -> IO TestTree
testTreeFromTest test = do
  testSpec test.name (mapM_ createTestCaseSpec test.testCases)
  where
    createTestCaseSpec :: TestCase -> Spec
    createTestCaseSpec testCase =
      it testCase.name $ do
        testCaseDir <- getTestCaseDir test.name testCase.name
        let testCaseCommand = createTestCaseCommand testCaseDir testCase
            testName = test.name ++ " / " ++ testCase.name

        setupTestCase testCaseDir
        (exitCode, logFile) <- executeTestCaseCommand testCaseDir testCaseCommand testName

        case exitCode of
          ExitFailure code -> expectationFailure =<< formatCommandFailure code logFile
          ExitSuccess -> cleanUpTestCase testCaseDir

createTestCaseCommand :: Path' Abs (Dir TestCaseDir) -> TestCase -> ShellCommand
createTestCaseCommand testCaseDir testCase =
  foldr1 (~&&) $ buildShellCommand testContext testCase.shellCommandBuilder
  where
    testContext = TestContext {testCaseDir, waspProjectContext}
    waspProjectContext =
      WaspProjectContext
        { waspProjectDir = testCaseDir </> (fromJust . parseRelDir $ "wasp-app"),
          waspProjectName = "wasp-app"
        }

setupTestCase :: Path' Abs (Dir TestCaseDir) -> IO ()
setupTestCase testCaseDir = do
  callCommand $ "rm -rf " ++ fromAbsDir testCaseDir
  callCommand $ "mkdir -p " ++ fromAbsDir testCaseDir

-- | Outputs of a passed test case are rarely needed and take up a lot of disk space
-- (mostly `node_modules`), so we remove them unless asked to keep them.
cleanUpTestCase :: Path' Abs (Dir TestCaseDir) -> IO ()
cleanUpTestCase testCaseDir = do
  shouldKeepOutputs <- isKeepOutputsEnabled
  unless shouldKeepOutputs $ removeDirectoryRecursive (fromAbsDir testCaseDir)
  where
    isKeepOutputsEnabled = maybe False (not . null) <$> lookupEnv keepOutputsEnvVarName
    keepOutputsEnvVarName = "WASP_E2E_TESTS_KEEP_OUTPUTS"

executeTestCaseCommand :: Path' Abs (Dir TestCaseDir) -> ShellCommand -> String -> IO (ExitCode, Path' Abs (File TestLogFile))
executeTestCaseCommand testCaseDir testCaseCommand testName = do
  let logFile = testCaseDir </> testCaseLogFileInTestCaseDir
  (logOut, logErr) <- openLogForCommand logFile testName testCaseCommand
  (_, _, _, processHandle) <-
    createProcess
      (shell testCaseCommand)
        { cwd = Just $ fromAbsDir testCaseDir,
          std_in = Inherit,
          std_out = UseHandle logOut,
          std_err = UseHandle logErr
        }
  exitCode <- waitForProcess processHandle
  return (exitCode, logFile)
