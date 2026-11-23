module Tests.WaspStudioTest (waspStudioTest) where

import qualified Data.Text as T
import E2E
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- | NOTE: Studio will probably get its own Playwright tests in its package,
-- so here we only test that the command works.
--
-- We stop the command like Ctrl+C in a terminal does, by interrupting its
-- whole process group, so the test only checks that this stops everything.
-- Unlike in 'Tests.WaspStartTest', we can't signal only Wasp: the command
-- doesn't take the project lock, so we can't read Wasp's PID from it.
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
          signalProcessGroupOf SIGINT studio
          waitForExit studio 60
          waitUntilNotListening [waspStudioPort] 30
    ]
