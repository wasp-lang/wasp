module Tests.WaspDbMigrateDevTest (waspDbMigrateDevTest) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf)
import E2E
import qualified StrongPath as SP
import System.Directory (listDirectory)
import Tests.Fixtures (taskPrismaModel)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Generator.DbGenerator.Common (dbMigrationsDirInDbRootDir, dbRootDirInGeneratedAppDir)
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedAppDirInDotWaspDir)
import Wasp.Project.Db.Migrations (dbMigrationsDirInWaspProjectDir)

-- | TODO: Test on all databases (e.g. Postgresql)
waspDbMigrateDevTest :: E2ETest
waspDbMigrateDevTest =
  testGroup
    "wasp-db-migrate-dev"
    [ test "fail-outside-project" $ waspFails ["db", "migrate-dev"],
      test "succeed-migrations-up-to-date"
        $ withNewWaspProject minimalStarterTemplate
        $ waspDbMigrateDev "no_migration",
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
        $ withTimeout (5 * 60)
        $ withOpenEmptyStdin
        $ waspCli ["db", "migrate-dev", "--name", "no_migration"]
    ]
  where
    -- In the project and in the generated app.
    assertMigrationExists :: String -> TestM ()
    assertMigrationExists migrationName =
      forM_ [SP.fromRelDir dbMigrationsDirInWaspProjectDir, SP.fromRelDir generatedAppMigrationsDir] $ \migrationsDir -> do
        migrations <- resolvePath migrationsDir >>= liftIO . listDirectory
        assertTrue
          ("there's a migration named " ++ show migrationName ++ " in " ++ migrationsDir)
          (any (migrationName `isInfixOf`) migrations)

    generatedAppMigrationsDir =
      dotWaspDirInWaspProjectDir
        SP.</> generatedAppDirInDotWaspDir
        SP.</> dbRootDirInGeneratedAppDir
        SP.</> dbMigrationsDirInDbRootDir
