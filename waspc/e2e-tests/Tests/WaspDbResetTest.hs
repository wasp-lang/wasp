module Tests.WaspDbResetTest (waspDbResetTest) where

import E2E
import Tests.Fixtures (TaskSeed (..), taskPrismaModel, taskSeedName, writeTaskSeedsApp)
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
          wasp ["compile"]
          appendToFile "schema.prisma" taskPrismaModel
          waspDbMigrateDev "foo"
          writeTaskSeedsApp [PopulateTasks, AssertTasksEmpty, AssertTasksNotEmpty]
          seed AssertTasksEmpty
          seed PopulateTasks
          seed AssertTasksNotEmpty
          run waspDbResetForce
          seed AssertTasksEmpty
    ]
  where
    seed taskSeed = wasp ["db", "seed", taskSeedName taskSeed]
