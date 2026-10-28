module Tests.WaspDbResetTest (waspDbResetTests) where

import E2e
import Tests.Fixtures (assertTasksEmptySeed, assertTasksNotEmptySeed, populateTasksSeed, writeTaskSeedsApp)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- | We seed the database ourselves, because Wasp skips seeding during the
-- reset (unlike Prisma).
waspDbResetTests :: [E2eTest]
waspDbResetTests =
  testGroup
    "wasp-db-reset"
    [ test "fail-outside-project" $ waspFails ["db", "reset"],
      test "succeed-reset-database" $
        withNewWaspProject minimalStarterTemplate $ do
          wasp ["compile"]
          writeTaskSeedsApp
          waspDbMigrateDev "foo"
          wasp ["db", "seed", assertTasksEmptySeed]
          wasp ["db", "seed", populateTasksSeed]
          wasp ["db", "seed", assertTasksNotEmptySeed]
          wasp ["db", "reset", "--force"]
          wasp ["db", "seed", assertTasksEmptySeed]
    ]
