module Tests.WaspDbStudioTest (test_waspDbStudio) where

import E2E
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- | NOTE: We don't test what Prisma Studio shows, since that's Prisma's feature.
test_waspDbStudio :: TestTree
test_waspDbStudio =
  testGroup
    "wasp-db-studio"
    [ test "fail-outside-project" $ waspFailsOutsideProject ["db", "studio"],
      test "succeed-uncompiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          port <- allocatePort
          -- Prisma Studio doesn't open a browser tab with this.
          dbStudio <- startInBackground . withEnv [("BROWSER", "none")] $ waspCli ["db", "studio", "--port", show port]
          waitUntilListening dbStudio [port] appStartTimeoutSeconds
          interruptProcessGroupAndWaitUntilStopped dbStudio [port]
    ]
