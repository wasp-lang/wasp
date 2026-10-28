module Tests.WaspDbStudioTest (waspDbStudioTests) where

import E2e
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- | NOTE: We don't test what Prisma Studio shows, since that's Prisma's feature.
waspDbStudioTests :: [E2eTest]
waspDbStudioTests =
  testGroup
    "wasp-db-studio"
    [ test "fail-outside-project" $ waspFails ["db", "studio"],
      usingResources [prismaStudioPort] $
        test "succeed-uncompiled-project" $
          withNewWaspProject minimalStarterTemplate $ do
            dbStudio <- startInBackground $ waspCli ["db", "studio"]
            waitForOutput dbStudio "localhost:5555" (5 * 60)
            interrupt dbStudio
            waitForExit dbStudio
    ]
