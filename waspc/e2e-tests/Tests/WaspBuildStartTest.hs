module Tests.WaspBuildStartTest (waspBuildStartTest) where

import E2E
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspBuildStartTest :: E2ETest
waspBuildStartTest =
  testGroup
    "wasp-build-start"
    [ test "fail-outside-project" $ waspFails ["build", "start"],
      test "fail-unbuilt-project" $
        withNewWaspProject minimalStarterTemplate $ do
          setDbToPostgres
          waspFails ["build", "start"] >>= assertOutputContains "You can build the app with the `wasp build` command.",
      -- `wasp build start` builds and runs the server's Docker image.
      test "succeed-built-project" $ do
        skipIfDockerDisabled
        withNewWaspProject minimalStarterTemplate $ do
          ports <- allocateAppPorts
          setDbToPostgres
          wasp ["build"]
          app <- startInBackground $ waspCli (["build", "start", "-s", "DATABASE_URL=none"] ++ appPortArgs ports)
          waitUntilListening app (appPortList ports) (15 * 60)
          interruptProcessGroupAndWaitUntilStopped app (appPortList ports)
    ]
