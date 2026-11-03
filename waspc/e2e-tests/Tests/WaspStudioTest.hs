module Tests.WaspStudioTest (waspStudioTest) where

import E2E
import qualified E2E.Ports as Ports
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- | NOTE: Studio will probably get its own Playwright tests in its package,
-- so here we only test that the command works.
waspStudioTest :: E2ETest
waspStudioTest =
  testGroup
    "wasp-studio"
    [ test "fail-outside-project" $ waspFails ["studio"],
      test "succeed-uncompiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          studio <- startInBackground $ waspCli ["studio"]
          -- The port is fixed, so we also check the output to know it's our Studio.
          waitForOutput studio "Server listening on port 4000" (5 * 60)
          waitUntilListening [Ports.waspStudioPort] 30
          interrupt studio
          waitForExit studio 60
          waitUntilNotListening [Ports.waspStudioPort] 30
    ]
