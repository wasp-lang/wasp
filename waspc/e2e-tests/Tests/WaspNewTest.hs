module Tests.WaspNewTest (test_waspNew) where

import qualified Data.Text as T
import E2E
import qualified StrongPath as SP
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (basicStarterTemplate, minimalStarterTemplate)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates (StarterTemplate)
import Wasp.Generator.DbGenerator.Common (dbRootDirInGeneratedAppDir)
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)
import Wasp.Project.Db (databaseUrlEnvVarName)
import Wasp.Project.Db.Dev.Sqlite (devDbFileInDbRootDir)
import Wasp.Project.Db.Migrations (dbMigrationsDirInWaspProjectDir)

test_waspNew :: TestTree
test_waspNew =
  testGroup
    "wasp-new"
    [ test "create-minimal" $
        withNewWaspProject minimalStarterTemplate $ do
          assertFileExists "main.wasp.ts"
          assertDirectoryDoesNotExist migrationsDir,
      test "create-minimal-with-piped-answers-fails" $ assertWaspNewFailsWithPipedAnswers minimalStarterTemplate,
      test "create-basic" $
        withNewWaspProject basicStarterTemplate $ do
          assertDirectoryExists migrationsDir
          assertFileExists devDbFile,
      test "create-basic-leaves-database-from-environment-alone" $ do
        result <- run . withEnv [(databaseUrlEnvVarName, "file:./unrelated.db")] $ waspCliNew basicStarterTemplate
        assertOutputContains "Migration skipped" result
        assertOutputContains "wasp db migrate-dev" result
        inDirectory waspAppDirName $ do
          assertFileExists "main.wasp.ts"
          assertDirectoryDoesNotExist migrationsDir,
      test "create-basic-with-piped-answers-fails" $ assertWaspNewFailsWithPipedAnswers basicStarterTemplate
      -- NOTE: We can't test the `saas` template here, since its "dev" tag doesn't exist.
    ]
  where
    migrationsDir = SP.fromRelDir dbMigrationsDirInWaspProjectDir
    devDbFile = SP.fromRelFile $ generatedAppDirInWaspProjectDir SP.</> dbRootDirInGeneratedAppDir SP.</> devDbFileInDbRootDir

    assertWaspNewFailsWithPipedAnswers :: StarterTemplate -> TestM ()
    assertWaspNewFailsWithPipedAnswers template =
      assertFailsWithoutInteractiveTerminal
        "wasp new"
        (waspCli ["new"]) {stdinMode = StdinText (T.pack $ waspAppDirName ++ "\n" ++ show template ++ "\n")}
