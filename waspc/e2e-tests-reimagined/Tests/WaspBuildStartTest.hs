module Tests.WaspBuildStartTest (waspBuildStartTests) where

import E2e
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspBuildStartTests :: [E2eTest]
waspBuildStartTests =
  testGroup
    "wasp-build-start"
    [ test "fail-outside-project" $ waspFails ["build", "start"],
      test "fail-unbuilt-project" $
        withNewWaspProject minimalStarterTemplate $ do
          setDbToPostgres
          waspFails ["build", "start"],
      usingResources [waspDevServerPorts] . requiringDocker $
        test "succeed-built-project" $
          withNewWaspProject minimalStarterTemplate $ do
            setDbToPostgres
            wasp ["build"]
            buildStart <- startInBackground $ waspCli ["build", "start", "-s", "DATABASE_URL=none"]
            waitForOutput buildStart "Server listening on port 3001" (15 * 60)
            interrupt buildStart
            waitForExit buildStart
    ]
