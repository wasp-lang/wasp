module Tests.WaspDbMigrateDevTest (test_waspDbMigrateDev) where

import Control.Monad (forM_)
import Data.List (isInfixOf)
import E2E
import GHC.Stack (HasCallStack)
import Tests.Fixtures (taskPrismaModel)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- | TODO: Test on all databases (e.g. Postgresql)
test_waspDbMigrateDev :: TestTree
test_waspDbMigrateDev =
  testGroup
    "wasp-db-migrate-dev"
    [ test "fail-outside-project" $ waspFailsOutsideProject ["db", "migrate-dev"],
      test "succeed-migrations-up-to-date" $
        withNewWaspProject minimalStarterTemplate $ do
          waspDbMigrateDev "no_migration"
          assertNoMigrationExists "no_migration",
      test "succeed-create-new-migration" $
        withNewWaspProject minimalStarterTemplate $ do
          appendToFile "schema.prisma" taskPrismaModel
          waspDbMigrateDev "yes_migration"
          assertMigrationExists "yes_migration",
      -- Wasp's DB connection check runs `prisma db execute --stdin`, which
      -- reads its stdin to the end. It used to inherit Wasp's stdin, so when
      -- that was a pipe nobody wrote to or closed, the check hung forever.
      test "succeed-with-stdin-open-but-empty"
        $ withNewWaspProject minimalStarterTemplate
        $ run
          (waspCli ["db", "migrate-dev", "--name", "no_migration"])
            { stdinMode = OpenEmptyStdin,
              timeoutSeconds = hangTimeoutSeconds
            }
          >>= assertOutputContains dbMigratedMessage
    ]
  where
    -- The regression hangs, so we don't wait for the default timeout.
    -- Migrating a new project takes much less.
    hangTimeoutSeconds :: Int
    hangTimeoutSeconds = 5 * 60

    -- In the project and in the generated app.
    assertMigrationExists, assertNoMigrationExists :: (HasCallStack) => String -> TestM ()
    assertMigrationExists migrationName =
      forM_ dbMigrationsDirs $ \migrationsDir -> do
        migrations <- listDbMigrations migrationsDir
        migrations `shouldSatisfy` any (migrationName `isInfixOf`)
    assertNoMigrationExists migrationName =
      forM_ dbMigrationsDirs $ \migrationsDir -> do
        migrations <- listDbMigrations migrationsDir
        migrations `shouldNotSatisfy` any (migrationName `isInfixOf`)
