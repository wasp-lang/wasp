module Tests.WaspSpecAvailableTest (waspSpecAvailableTest) where

import Control.Monad (forM_, void)
import qualified Data.Text as T
import E2e
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Util.Terminal (styleCode)

waspSpecAvailableTest :: E2eTest
waspSpecAvailableTest =
  testGroup
    "wasp-spec-available"
    [ test "commands-requiring-wasp-spec-fail-with-install-hint-when-missing" $
        withNewWaspProject minimalStarterTemplate $ do
          removeNodeModules
          mapM_
            assertFailsWithInstallHint
            [ waspCli ["compile"],
              waspCli ["build"],
              waspCli ["show", "spec"],
              waspCli ["deps"],
              waspCli ["dockerfile"],
              waspCli ["studio"],
              waspDbResetForce,
              waspCli ["deploy", "fly", "setup"],
              waspCli ["start", "db"],
              waspCli ["start"]
            ],
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
      test "commands-not-requiring-wasp-spec-succeed-when-missing"
        $ withNewWaspProject minimalStarterTemplate
        $ forM_
          [ -- Project-scoped commands that don't require wasp-spec to be installed.
            ["clean"],
            ["install"],
            ["show", "build"],
            -- Project-agnostic commands. They don't read the project, so
            -- they should be unaffected by wasp-spec presence.
            ["version"],
            ["completion"],
            ["telemetry"],
            ["news"]
          ]
        $ \args -> removeNodeModules >> wasp args
    ]
  where
    -- Puts the project in a "wasp-spec missing" state without invoking any wasp command.
    removeNodeModules :: TestM ()
    removeNodeModules = deleteDirectory "node_modules"

    corruptWaspSpecVersion :: TestM ()
    corruptWaspSpecVersion = inDirectory ".wasp/spec" $ void $ run npmPkgSetVersion

    assertFailsWithInstallHint :: Command -> TestM ()
    assertFailsWithInstallHint cmd =
      runExpectingFailure cmd >>= assertOutputContains (T.pack $ "Run " ++ styleCode "wasp install")

{- HLINT ignore npmPkgSetVersion "Avoid npm and npx" -}
npmPkgSetVersion :: Command
npmPkgSetVersion = command "npm" ["pkg", "set", "version=9.9.9"]
