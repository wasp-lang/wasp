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

-- | TODO: Runs... 
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
          
        sequenceTestCases :: [EphemeralTestCase] -> Spec
        sequenceTestCases testCases = do
          forM_ testCases $ \testCase -> do
            let testCaseName = _ephemeralTestCaseName testCase
            let testCaseCommand = buildShellCommand ephemeralTestContext (_ephemeralTestCaseCommandBuilder testCase)
            
            -- Force sequential execution of the commands
            -- TODO: higher versions of Test.Hspec support sequential execution
            (exitCode, _stdOut, stdErr) <- runIO $ do
              putStrLn $ "Executing test case: " ++ testCaseCommand
              readCreateProcessWithExitCode (shell ("cd " ++ fromAbsDir ephemeralDir ~&& testCaseCommand)) ""
            
            it testCaseName $ do
              case exitCode of
                ExitFailure _ -> expectationFailure stdErr
                ExitSuccess -> return ()
          
          -- Clean up the ephemeral directory only if all test cases have passed
          runIO $ callCommand $ "rm -rf " ++ fromAbsDir ephemeralDir

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
