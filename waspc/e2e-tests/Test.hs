{-# LANGUAGE DuplicateRecordFields #-}

module Test
  ( Test (..),
    TestCase (..),
    testTreeFromTest,
  )
where

import Data.Maybe (fromJust)
import FileSystem (TestCaseDir, TestLogFile, getTestCaseDir, testCaseLogFileInTestCaseDir)
import ShellCommands (ShellCommand, ShellCommandBuilder, TestContext (..), WaspProjectContext (..), buildShellCommand, (~&&))
import StrongPath (Abs, Dir, File, Path', fromAbsDir, fromAbsFile, parseRelDir, (</>))
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (..), StdStream (..), callCommand, createProcess, shell, waitForProcess)
import Test.Hspec (Spec, expectationFailure, it)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import TestLogging (openLogForCommand)

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
            logFile = testCaseDir </> testCaseLogFileInTestCaseDir
            testName = test.name ++ " / " ++ testCase.name

        setupTestCase testCaseDir
        exitCode <- executeTestCaseCommand testCaseDir testCaseCommand logFile testName

        case exitCode of
          ExitFailure code ->
            expectationFailure $ "Command failed with exit code " ++ show code ++ ". See log: " ++ fromAbsFile logFile
          ExitSuccess -> return ()

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

executeTestCaseCommand :: Path' Abs (Dir TestCaseDir) -> ShellCommand -> Path' Abs (File TestLogFile) -> String -> IO ExitCode
executeTestCaseCommand testCaseDir testCaseCommand logFile testName = do
  (logOut, logErr) <- openLogForCommand logFile testName testCaseCommand
  (_, _, _, processHandle) <-
    createProcess
      (shell testCaseCommand)
        { cwd = Just $ fromAbsDir testCaseDir,
          std_in = Inherit,
          std_out = UseHandle logOut,
          std_err = UseHandle logErr
        }
  waitForProcess processHandle
