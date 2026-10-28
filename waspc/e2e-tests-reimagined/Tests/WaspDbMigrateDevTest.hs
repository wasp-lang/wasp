module Tests.WaspDbMigrateDevTest (waspDbMigrateDevTests) where

import E2e
import Tests.Fixtures (taskPrismaModel)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- TODO: Test on all databases (e.g. PostgreSQL).
waspDbMigrateDevTests :: [E2eTest]
waspDbMigrateDevTests =
  testGroup
    "wasp-db-migrate-dev"
    [ test "fail-outside-project" $ waspFails ["db", "migrate-dev"],
      test "succeed-migrations-up-to-date"
        $ withNewWaspProject minimalStarterTemplate
        $ waspDbMigrateDev "no_migration",
      test "succeed-create-new-migration" $
        withNewWaspProject minimalStarterTemplate $ do
          appendToPrismaSchema taskPrismaModel
          waspDbMigrateDev "yes_migration"
          assertMigrationExists "yes_migration",
      -- Wasp's DB connection check runs `prisma db execute --stdin`, which reads
      -- its stdin to the end. It used to inherit Wasp's stdin, so when that was
      -- a pipe nobody wrote to or closed, the check hung forever.
      test "succeed-with-stdin-open-but-empty"
        $ withNewWaspProject minimalStarterTemplate
        $ run . withTimeout (5 * 60) . withOpenEmptyStdin
        $ waspCli ["db", "migrate-dev", "--name", "no_migration"]
    ]
