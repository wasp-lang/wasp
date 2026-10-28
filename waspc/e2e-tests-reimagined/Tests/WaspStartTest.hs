module Tests.WaspStartTest (waspStartTests) where

import Control.Monad (void)
import E2e
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspStartTests :: [E2eTest]
waspStartTests =
  testGroup
    "wasp-start"
    [ test "fail-outside-project" $ waspFails ["start"],
      usingResources [waspDevServerPorts] $
        test "succeed-uncompiled-project" $
          withNewWaspProject minimalStarterTemplate $ do
            startAndStopWaspApp
            assertDirectoryExists ".wasp"
            assertDirectoryExists "node_modules",
      usingResources [waspDevServerPorts] $
        test "succeed-compiled-project" $
          withNewWaspProject minimalStarterTemplate $ do
            wasp ["compile"]
            startAndStopWaspApp
    ]
  where
    startAndStopWaspApp :: TestM ()
    startAndStopWaspApp = do
      waspStart <- startInBackground $ waspCli ["start"]
      waitForOutput waspStart "Server listening on port 3001" (10 * 60)
      -- Vite prints this once the client dev server is up.
      waitForOutput waspStart "ready in" 60
      interrupt waspStart
      void $ waitForExit waspStart
