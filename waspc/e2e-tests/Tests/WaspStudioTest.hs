module Tests.WaspStudioTest (waspStudioTest) where

import qualified Data.Text as T
import E2E
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
          waitForOutput studio ("Server listening on port " <> T.pack (show waspStudioPort)) (5 * 60)
          waitUntilListening studio [waspStudioPort] 30
          interruptProcessGroupAndWaitUntilStopped studio [waspStudioPort]
    ]
