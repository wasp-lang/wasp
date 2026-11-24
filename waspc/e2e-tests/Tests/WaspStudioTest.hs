module Tests.WaspStudioTest (waspStudioTest) where

import E2E
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- | NOTE: Studio will probably get its own Playwright tests in its package,
-- so here we only test that the command works.
waspStudioTest :: E2ETest
waspStudioTest =
  testGroup
    "wasp-studio"
    [ test "fail-outside-project" $ waspFailsOutsideProject ["studio"],
      test "succeed-uncompiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          port <- allocatePort
          studio <- startInBackground $ waspCli ["studio", "--port", show port]
          waitUntilListening studio [port] appStartTimeoutSeconds
          interruptProcessGroupAndWaitUntilStopped studio [port]
    ]
