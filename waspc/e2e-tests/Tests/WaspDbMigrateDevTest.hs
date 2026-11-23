module Tests.WaspDbMigrateDevTest (waspDbMigrateDevTest) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf)
import E2E
import System.Directory (doesDirectoryExist, listDirectory)
import Tests.Fixtures (taskPrismaModel)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- | TODO: Test on all databases (e.g. Postgresql)
waspDbMigrateDevTest :: E2ETest
waspDbMigrateDevTest =
  testGroup
    "wasp-db-migrate-dev"
    [ test "fail-outside-project" $ waspFails ["db", "migrate-dev"],
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
              timeoutSeconds = 5 * 60
            }
          >>= assertOutputContains "Database successfully migrated."
    ]
  where
    -- In the project and in the generated app.
    assertMigrationExists :: String -> TestM ()
    assertMigrationExists migrationName =
      forM_ dbMigrationsDirs $ \migrationsDir -> do
        migrations <- listMigrations migrationsDir
        assertTrue
          ("there's a migration named " ++ show migrationName ++ " in " ++ migrationsDir)
          (any (migrationName `isInfixOf`) migrations)

    -- In the project and in the generated app.
    assertNoMigrationExists :: String -> TestM ()
    assertNoMigrationExists migrationName =
      forM_ dbMigrationsDirs $ \migrationsDir -> do
        migrations <- listMigrations migrationsDir
        assertTrue
          ("there's no migration named " ++ show migrationName ++ " in " ++ migrationsDir)
          (not $ any (migrationName `isInfixOf`) migrations)

    -- Without migrations, there might be no migrations dir.
    listMigrations :: FilePath -> TestM [FilePath]
    listMigrations migrationsDir = do
      absMigrationsDir <- resolvePath migrationsDir
      liftIO $ do
        exists <- doesDirectoryExist absMigrationsDir
        if exists then listDirectory absMigrationsDir else return []
