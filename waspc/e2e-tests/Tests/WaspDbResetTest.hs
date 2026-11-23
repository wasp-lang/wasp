module Tests.WaspDbResetTest (waspDbResetTest) where

import E2E
import Tests.Fixtures (TaskSeed (..), allTaskSeeds, runTaskSeed, setUpProjectWithTaskSeeds)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- | We include seeds in the test, because Wasp skips the seeding during the
-- reset (unlike Prisma), and seeds are how we check the database's state.
waspDbResetTest :: E2ETest
waspDbResetTest =
  testGroup
    "wasp-db-reset"
    [ test "fail-outside-project" $ waspFails ["db", "reset"],
      test "succeed-reset-database" $
        withNewWaspProject minimalStarterTemplate $ do
          setUpProjectWithTaskSeeds allTaskSeeds
          runTaskSeed AssertTasksEmpty
          runTaskSeed PopulateTasks
          runTaskSeed AssertTasksNotEmpty
          run waspCliDbResetForce
          runTaskSeed AssertTasksEmpty
    ]
