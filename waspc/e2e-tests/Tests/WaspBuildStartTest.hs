module Tests.WaspBuildStartTest (waspBuildStartTest) where

import E2e
import E2e.Ports (appPortArgs, appPortList)
import qualified E2e.Ports as Ports
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspBuildStartTest :: E2eTest
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
          let ports = Ports.waspBuildStart
          setDbToPostgres
          wasp ["build"]
          app <- startInBackground $ waspCli (["build", "start", "-s", "DATABASE_URL=none"] ++ appPortArgs ports)
          waitUntilListening (appPortList ports) (15 * 60)
          interrupt app
          waitForExit app 60
          waitUntilNotListening (appPortList ports) 30
    ]
