module Tests.WaspDbStudioTest (waspDbStudioTest) where

import qualified Data.Text as T
import E2E
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- | NOTE: We don't test what Prisma Studio shows, since that's Prisma's feature.
--
-- We stop the command like Ctrl+C in a terminal does, by interrupting its
-- whole process group, so the test only checks that this stops everything.
-- Unlike in 'Tests.WaspStartTest', we can't signal only Wasp: the command
-- doesn't take the project lock, so we can't read Wasp's PID from it.
waspDbStudioTest :: E2ETest
waspDbStudioTest =
  testGroup
    "wasp-db-studio"
    [ test "fail-outside-project" $ waspFails ["db", "studio"],
      test "succeed-uncompiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          -- Prisma Studio doesn't open a browser tab with this.
          dbStudio <- startInBackground . withEnv [("BROWSER", "none")] $ waspCli ["db", "studio"]
          -- The port is fixed, so we also check the output to know it's our Prisma Studio.
          waitForOutput dbStudio ("localhost:" <> T.pack (show prismaStudioPort)) (5 * 60)
          waitUntilListening dbStudio [prismaStudioPort] 30
          signalProcessGroupOf SIGINT dbStudio
          waitForExit dbStudio 60
          waitUntilNotListening [prismaStudioPort] 30
    ]
