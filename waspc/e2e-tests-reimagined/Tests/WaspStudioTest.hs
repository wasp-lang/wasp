module Tests.WaspStudioTest (waspStudioTests) where

import E2e
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- | NOTE: Studio will probably get its own Playwright tests in its package,
-- so here we only test that the command works.
waspStudioTests :: [E2eTest]
waspStudioTests =
  testGroup
    "wasp-studio"
    [ test "fail-outside-project" $ waspFails ["studio"],
      usingResources [waspStudioPort] $
        test "succeed-uncompiled-project" $
          withNewWaspProject minimalStarterTemplate $ do
            waspStudio <- startInBackground $ waspCli ["studio"]
            waitForOutput waspStudio "Server listening on port 4000" (5 * 60)
            interrupt waspStudio
            waitForExit waspStudio
    ]
