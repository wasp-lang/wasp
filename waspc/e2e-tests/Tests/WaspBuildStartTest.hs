module Tests.WaspBuildStartTest (waspBuildStartTest) where

import E2E
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- | We stop the command like Ctrl+C in a terminal does, by interrupting its
-- whole process group, so the test only checks that this stops everything.
-- Unlike in 'Tests.WaspStartTest', we can't signal only Wasp: the command
-- doesn't take the project lock, so we can't read Wasp's PID from it.
waspBuildStartTest :: E2ETest
waspBuildStartTest =
  testGroup
    "wasp-build-start"
    [ test "fail-outside-project" $ waspFails ["build", "start"],
      test "fail-unbuilt-project" $
        withNewWaspProject minimalStarterTemplate $ do
          setDbToPostgres
          waspFails ["build", "start"],
      -- `wasp build start` builds and runs the server's Docker image.
      test "succeed-built-project" $ do
        skipIfDockerDisabled
        withNewWaspProject minimalStarterTemplate $ do
          ports <- allocateAppPorts
          setDbToPostgres
          wasp ["build"]
          app <- startInBackground $ waspCli (["build", "start", "-s", "DATABASE_URL=none"] ++ appPortArgs ports)
          waitUntilListening app (appPortList ports) (15 * 60)
          signalProcessGroupOf SIGINT app
          waitForExit app 60
          waitUntilNotListening (appPortList ports) 30
    ]
