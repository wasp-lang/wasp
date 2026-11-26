module Tests.WaspDbResetTest (test_waspDbReset) where

import E2E
import Tests.Fixtures (allTaskSeeds, assertTasksEmptySeed, assertTasksNotEmptySeed, populateTasksSeed, runSeed, setUpProjectWithTaskSeeds)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- | We include seeds in the test, because Wasp skips the seeding during the
-- reset (unlike Prisma), and seeds are how we check the database's state.
test_waspDbReset :: TestTree
test_waspDbReset =
  testGroup
    "wasp-db-reset"
    [ test "fail-outside-project" $ waspFailsOutsideProject ["db", "reset"],
      test "succeed-reset-database" $
        withNewWaspProject minimalStarterTemplate $ do
          setUpProjectWithTaskSeeds allTaskSeeds
          runSeed assertTasksEmptySeed
          runSeed populateTasksSeed
          runSeed assertTasksNotEmptySeed
          run waspCliDbResetForce
          runSeed assertTasksEmptySeed
    ]
