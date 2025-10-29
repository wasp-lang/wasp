module EphemeralTest
  ( EphemeralTest,
    EphemeralTestCase,
    makeEphemeralTest,
    makeEphemeralTestCase,
    runEphemeralTest,
  )
where

import Control.Monad (forM_)
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
import qualified Data.Type.Bool as Spec

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
  getEphemeralDir ephemeralTestName >>= executeEphemeralTestWorkflow
  where
    ephemeralTestName :: String
    ephemeralTestName = _ephemeralTestName ephemeralTest

    executeEphemeralTestWorkflow :: Path' Abs (Dir EphemeralDir) -> IO TestTree
    executeEphemeralTestWorkflow ephemeralDir = do
      setupEphemeralTest
      executeEphemeralTestCases

      where
        setupEphemeralTest :: IO ()
        setupEphemeralTest = do
          callCommand $ "rm -rf " ++ fromAbsDir ephemeralDir
          callCommand $ "mkdir " ++ fromAbsDir ephemeralDir

        executeEphemeralTestCases :: IO TestTree
        executeEphemeralTestCases = do
          putStrLn $ "Executing ephemeral test: " ++ ephemeralTestName
          testSpec ephemeralTestName $ sequenceTestCases (_ephemeralTestCases ephemeralTest)
        
        -- | Runs ephemeral test cases in sequence.
        -- Short-circuits when any of the tests fails, since we rely on sequential execution.
        sequenceTestCases :: [EphemeralTestCase] -> Spec
        sequenceTestCases testCases = do
          sequenceTestCases' testCases
          where
            sequenceTestCases' [] = return ()
            sequenceTestCases' (testCase : restTestCases) = do
              let testCaseName = _ephemeralTestCaseName testCase
              let testCaseCommand = buildShellCommand ephemeralTestContext (_ephemeralTestCaseCommandBuilder testCase)
              
              (exitCode, _stdOut, stdErr) <- runIO $ do
                putStrLn $ "Executing test case: " ++ testCaseCommand
                readCreateProcessWithExitCode (shell ("cd " ++ fromAbsDir ephemeralDir ~&& testCaseCommand)) ""
              
              it testCaseName $ do
                case exitCode of
                  ExitFailure _ -> expectationFailure stdErr
                  ExitSuccess -> return ()
              
              case exitCode of
                ExitSuccess -> sequenceTestCases' restTestCases
                ExitFailure _ -> return ()

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
