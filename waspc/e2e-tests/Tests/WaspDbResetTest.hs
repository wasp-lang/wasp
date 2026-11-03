module Tests.WaspDbResetTest (waspDbResetTest) where

import E2e
import Tests.Fixtures (assertTasksEmptySeed, assertTasksNotEmptySeed, populateTasksSeed, taskPrismaModel, writeTaskSeedsApp)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- | We include seeds in the test, because Wasp skips the seeding during the
-- reset (unlike Prisma), and seeds are how we check the database's state.
waspDbResetTest :: E2eTest
waspDbResetTest =
  testGroup
    "wasp-db-reset"
    [ test "fail-outside-project" $ waspFails ["db", "reset"],
      test "succeed-reset-database" $
        withNewWaspProject minimalStarterTemplate $ do
          wasp ["compile"]
          appendToFile "schema.prisma" taskPrismaModel
          waspDbMigrateDev "foo"
          writeTaskSeedsApp
          wasp ["db", "seed", assertTasksEmptySeed]
          wasp ["db", "seed", populateTasksSeed]
          wasp ["db", "seed", assertTasksNotEmptySeed]
          run waspDbResetForce
          wasp ["db", "seed", assertTasksEmptySeed]
    ]
