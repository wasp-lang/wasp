module Tests.WaspDbStudioTest (waspDbStudioTest) where

import E2e
import qualified E2e.Ports as Ports
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- | NOTE: We don't test what Prisma Studio shows, since that's Prisma's feature.
waspDbStudioTest :: E2eTest
waspDbStudioTest =
  testGroup
    "wasp-db-studio"
    [ test "fail-outside-project" $ waspFails ["db", "studio"],
      test "succeed-uncompiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          dbStudio <- startInBackground $ waspCli ["db", "studio"]
          -- The port is fixed, so we also check the output to know it's our Prisma Studio.
          waitForOutput dbStudio "localhost:5555" (5 * 60)
          waitUntilListening [Ports.prismaStudioPort] 30
          interrupt dbStudio
          waitForExit dbStudio 60
          waitUntilNotListening [Ports.prismaStudioPort] 30
    ]
