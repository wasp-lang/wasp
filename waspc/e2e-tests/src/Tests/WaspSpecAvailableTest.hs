module Tests.WaspSpecAvailableTest (test_waspSpecAvailable) where

import Control.Monad (forM_, void)
import qualified Data.Text as T
import E2E
import qualified StrongPath as SP
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.NodePackageFFI (InstallablePackage (WaspSpecPackage), getPackageInstallationPathInProject)
import Wasp.Util.Terminal (styleCode)

test_waspSpecAvailable :: TestTree
test_waspSpecAvailable =
  testGroup
    "wasp-spec-available"
    [ test "commands-requiring-wasp-spec-fail-with-install-hint-when-missing" $
        withNewWaspProject minimalStarterTemplate $ do
          removeNodeModules
          forM_
            [ waspCli ["compile"],
              waspCli ["build"],
              waspCli ["show", "spec"],
              waspCli ["deps"],
              waspCli ["dockerfile"],
              waspCli ["studio"],
              waspCliDbResetForce,
              waspCli ["deploy", "fly", "setup"],
              waspCli ["start", "db"],
              waspCli ["start"]
            ]
            assertFailsWithInstallHint,
      test "compile-fails-with-install-hint-when-wasp-spec-version-mismatches-cli" $
        withNewWaspProject minimalStarterTemplate $ do
          corruptWaspSpecVersion
          assertFailsWithInstallHint $ waspCli ["compile"],
      -- `wasp build start` requires `.wasp/build` to exist before its
      -- `WaspSpecAvailable` check fires, so we build first, then nuke
      -- node_modules to reach the install-hint failure path.
      test "build-start-fails-with-install-hint-when-wasp-spec-missing" $
        withNewWaspProject minimalStarterTemplate $ do
          setDbToPostgres
          wasp ["build"]
          removeNodeModules
          assertFailsWithInstallHint $ waspCli ["build", "start"],
      test "commands-not-requiring-wasp-spec-succeed-when-missing" $
        withNewWaspProject minimalStarterTemplate $ do
          forM_
            [ -- Project-scoped commands that don't require wasp-spec to be installed.
              ["clean"],
              ["install"],
              ["show", "build"],
              -- Project-agnostic commands. They don't read the project, so
              -- they should be unaffected by wasp-spec presence.
              ["version"],
              ["completion"],
              ["telemetry"]
            ]
            $ \args -> removeNodeModules >> wasp args,
      -- `wasp news` fetches the news. We point it at a server that isn't
      -- there, so the test works offline: then it must fail at fetching,
      -- after it checked any requirements.
      test "news-gets-past-requirements-when-wasp-spec-missing" $
        withNewWaspProject minimalStarterTemplate $ do
          removeNodeModules
          runExpectingFailure (withEnv [("WASP_NEWS_SERVER_URL", "http://localhost:1")] $ waspCli ["news"])
            >>= assertOutputContains "Getting Wasp news failed"
    ]
  where
    -- Puts the project in a "wasp-spec missing" state without invoking any wasp command.
    removeNodeModules :: TestM ()
    removeNodeModules = deleteDirectory "node_modules"

    corruptWaspSpecVersion :: TestM ()
    corruptWaspSpecVersion =
      inDirectory (SP.fromRelDir $ getPackageInstallationPathInProject WaspSpecPackage)
        $ void
        $ run npmPkgSetVersionCommand

    -- The commands should fail on the missing wasp-spec before doing any
    -- work (building, serving the app).
    assertFailsWithInstallHint :: Command -> TestM ()
    assertFailsWithInstallHint cmd =
      runExpectingFailure cmd {timeoutSeconds = failFastTimeoutSeconds}
        >>= assertOutputContains (T.pack $ "Run " ++ styleCode "wasp install")

{- HLINT ignore npmPkgSetVersionCommand "Avoid npm and npx" -}
npmPkgSetVersionCommand :: Command
npmPkgSetVersionCommand = program "npm" ["pkg", "set", "version=9.9.9"]
