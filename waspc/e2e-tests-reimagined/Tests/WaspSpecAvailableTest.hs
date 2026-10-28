module Tests.WaspSpecAvailableTest (waspSpecAvailableTests) where

import qualified Data.Text as T
import E2e
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Util.Terminal (styleCode)

waspSpecAvailableTests :: [E2eTest]
waspSpecAvailableTests =
  testGroup
    "wasp-spec-available"
    [ test "commands-requiring-wasp-spec-fail-with-install-hint-when-missing" $
        withNewWaspProject minimalStarterTemplate $ do
          deleteDirectory "node_modules"
          mapM_
            waspFailsWithInstallHint
            [ ["compile"],
              ["build"],
              ["show", "spec"],
              ["deps"],
              ["dockerfile"],
              ["studio"],
              ["db", "reset", "--force"],
              ["deploy", "fly", "setup"],
              ["start", "db"],
              ["start"],
              ["test", "client"]
            ],
      test "compile-fails-with-install-hint-when-wasp-spec-version-mismatches-cli" $
        withNewWaspProject minimalStarterTemplate $ do
          inDirectory ".wasp/spec" $ run $ command "npm" ["pkg", "set", "version=9.9.9"]
          waspFailsWithInstallHint ["compile"],
      -- `wasp build start` requires `.wasp/build` to exist before its
      -- `WaspSpecAvailable` check fires, so we build first and then delete
      -- node_modules to reach the install hint.
      test "build-start-fails-with-install-hint-when-wasp-spec-missing" $
        withNewWaspProject minimalStarterTemplate $ do
          setDbToPostgres
          wasp ["build"]
          deleteDirectory "node_modules"
          waspFailsWithInstallHint ["build", "start"],
      test "commands-not-requiring-wasp-spec-succeed-when-missing"
        $ withNewWaspProject minimalStarterTemplate
        $ mapM_
          (\args -> deleteDirectory "node_modules" >> wasp args)
          [ -- Project-scoped commands that don't require wasp-spec to be installed.
            ["clean"],
            ["install"],
            ["show", "build"],
            -- Project-agnostic commands. They don't read the project,
            -- so they should be unaffected by wasp-spec presence.
            ["version"],
            ["completion"],
            ["telemetry"],
            ["news"]
          ]
    ]
  where
    waspFailsWithInstallHint :: [String] -> TestM ()
    waspFailsWithInstallHint args =
      waspFails args >>= assertOutputContains (T.pack $ "Run " ++ styleCode "wasp install")
