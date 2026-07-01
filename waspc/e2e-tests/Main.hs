import FileSystem (getWaspcDirPath, waspCliDevToolInWaspcDir)
import SnapshotTest (testTreeFromSnapshotTest)
import StrongPath ((</>))
import qualified StrongPath as SP
import System.Environment (lookupEnv, setEnv)
import System.Info (os)
import System.Process (callCommand)
import Test (testTreeFromTest)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Tests.SnapshotTests.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest)
import Tests.SnapshotTests.WaspBuildSnapshotTest (waspBuildSnapshotTest)
import Tests.SnapshotTests.WaspCompileSnapshotTest (waspCompileSnapshotTest)
import Tests.SnapshotTests.WaspMigrateSnapshotTest (waspMigrateSnapshotTest)
import Tests.SnapshotTests.WaspNewSnapshotTest (waspNewSnapshotTest)
import Tests.ViteBuildTest (viteBuildTest)
import Tests.ViteConfigTest (viteConfigTest)
import Tests.WaspBuildTest (waspBuildTest)
import Tests.WaspCleanTest (waspCleanTest)
import Tests.WaspCompileTest (waspCompileTest)
import Tests.WaspCompletionTest (waspCompletionTest)
import Tests.WaspDbMigrateDevTest (waspDbMigrateDevTest)
import Tests.WaspDbResetTest (waspDbResetTest)
import Tests.WaspDbSeedTest (waspDbSeedTest)
import Tests.WaspDepsTest (waspDepsTest)
import Tests.WaspDockerfileTest (waspDockerfileTest)
import Tests.WaspInfoTest (waspInfoTest)
import Tests.WaspInstallTest (waspInstallTest)
import Tests.WaspNewTest (waspNewTest)
import Tests.WaspSpecAvailableTest (waspSpecAvailableTest)
import Tests.WaspSpecEntityTypesTest (waspSpecEntityTypesTest)
import Tests.WaspTelemetryTest (waspTelemetryTest)
import Tests.WaspTsSpecNodeEnvTest (waspTsSpecNodeEnvTest)
import Tests.WaspVersionTest (waspVersionTest)

main :: IO ()
main = do
  if os == "mingw32"
    then putStrLn "Skipping end-to-end tests on Windows due to tests using *nix-only commands"
    else do
      ensureE2eTestsEnvironment
      warmUpWaspCli
      defaultMain e2eTests

ensureE2eTestsEnvironment :: IO ()
ensureE2eTestsEnvironment = do
  existing <- lookupEnv "WASP_CLI_CMD"
  case existing of
    Just _ -> return ()
    Nothing -> do
      -- Runs the tests using the current state of the `waspc` project.
      waspcDir <- getWaspcDirPath
      let devWaspCliCmd = SP.fromAbsFile (waspcDir </> waspCliDevToolInWaspcDir)
      setEnv "WASP_CLI_CMD" devWaspCliCmd

-- | Builds the dev Wasp CLI once, serially, before the tests start invoking it
-- concurrently (tasty runs the test cases in parallel).
--
-- The dev CLI runs through `cabal run`, and Cabal does not support several
-- concurrent invocations sharing a single `dist-newstyle`: if the first build
-- isn't finished, the parallel invocations race to register the inplace package
-- and fail with "package.conf.inplace already exists". Doing one serial
-- invocation here forces that build to complete first, so the concurrent
-- invocations only ever run the already-built CLI.
warmUpWaspCli :: IO ()
warmUpWaspCli = callCommand "$WASP_CLI_CMD version > /dev/null"

-- TODO: Investigate automatically discovering the tests.
e2eTests :: TestTree
e2eTests =
  testGroup
    "E2E tests"
    [ testGroup
        "Snapshot Tests"
        ( map
            testTreeFromSnapshotTest
            [ waspNewSnapshotTest,
              waspCompileSnapshotTest,
              waspBuildSnapshotTest,
              waspMigrateSnapshotTest,
              kitchenSinkSnapshotTest
            ]
        ),
      testGroup
        "Tests"
        ( map
            testTreeFromTest
            [ -- general Wasp commads
              waspNewTest,
              waspTelemetryTest,
              waspCompletionTest,
              waspVersionTest,
              -- Wasp project commands
              waspCompileTest,
              -- NOTE(Franjo): The following tests have the `FIXME` comment because they
              -- are long running processes, which we haven't implmemented support for yet.
              -- Adding support should now be a matter of building on 'Command.startCommand'
              -- (e.g. a step that starts a command, waits for some output, and terminates it).
              -- FIXME: waspStartTest,
              waspBuildTest,
              waspTsSpecNodeEnvTest,
              viteBuildTest,
              viteConfigTest,
              -- FIXME: waspBuildStartTest,
              waspCleanTest,
              waspSpecAvailableTest,
              waspInfoTest,
              waspInstallTest,
              waspDepsTest,
              waspDockerfileTest,
              -- FIXME: waspStudioTest,
              -- Wasp project db commands
              -- FIXME: waspDbStartTest,
              -- FIXME: waspDbStudioTest,
              waspDbSeedTest,
              waspDbResetTest,
              waspDbMigrateDevTest,
              waspSpecEntityTypesTest
            ]
        )
    ]
