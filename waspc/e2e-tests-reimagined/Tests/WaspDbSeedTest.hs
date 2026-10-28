module Tests.WaspDbSeedTest (waspDbSeedTests) where

import E2e
import Tests.Fixtures (assertTasksEmptySeed, assertTasksNotEmptySeed, populateTasksSeed, writeTaskSeedsApp)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspDbSeedTests :: [E2eTest]
waspDbSeedTests =
  testGroup
    "wasp-db-seed"
    [ test "fail-outside-project" $ waspFails ["db", "seed"],
      test "succeed-seed-database" $
        withNewWaspProject minimalStarterTemplate $ do
          wasp ["compile"]
          writeTaskSeedsApp
          waspDbMigrateDev "foo"
          wasp ["db", "seed", assertTasksEmptySeed]
          wasp ["db", "seed", populateTasksSeed]
          wasp ["db", "seed", assertTasksNotEmptySeed]
    ]
