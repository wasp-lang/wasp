module Tests.WaspNewTest (waspNewTest) where

import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext,
    assertCommandOutputContainsAll,
    createTestWaspProject,
    inTestWaspProjectDir,
    waspCliNewWithPipedAnswersFails,
  )
import StrongPath (Dir, File, Path', Rel, (</>))
import qualified StrongPath as SP
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (basicStarterTemplate, minimalStarterTemplate)
import Wasp.Generator.DbGenerator.Common (dbRootDirInGeneratedAppDir)
import Wasp.Project.Common (WaspProjectDir, generatedAppDirInWaspProjectDir)
import Wasp.Project.Db.Dev.Sqlite (devDbFileInDbRootDir)
import Wasp.Project.Db.Migrations (dbMigrationsDirInWaspProjectDir)

waspNewTest :: Test
waspNewTest =
  Test
    "wasp-new"
    [ TestCase
        "create-minimal"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir [assertDirectoryDoesNotExist dbMigrationsDirInWaspProjectDir]
            ]
        ),
      TestCase
        "create-minimal-with-piped-answers-fails"
        (sequence [waspCliNewWithPipedAnswersFails "wasp-app" minimalStarterTemplate]),
      TestCase
        "create-basic"
        ( sequence
            [ createTestWaspProject basicStarterTemplate,
              inTestWaspProjectDir
                [ assertDirectoryExists dbMigrationsDirInWaspProjectDir,
                  assertFileExists devDbFileInWaspProjectDir
                ]
            ]
        ),
      TestCase
        "create-basic-leaves-database-from-environment-alone"
        ( sequence
            [ assertCommandOutputContainsAll
                (("DATABASE_URL=file:./unrelated.db " ++) <$> createTestWaspProject basicStarterTemplate)
                ["Migration skipped", "npm run wasp -- db migrate-dev"],
              inTestWaspProjectDir [assertDirectoryDoesNotExist dbMigrationsDirInWaspProjectDir]
            ]
        ),
      TestCase
        "create-basic-with-piped-answers-fails"
        (sequence [waspCliNewWithPipedAnswersFails "wasp-app" basicStarterTemplate])
        -- FIXME(Franjo): These will fail because there "dev" opensaas template tag doesn't exist.
        -- We can't really test the `saas` template in `waspc` e2e tests.
        --   TestCase
        --   "create-saas"
        --   (sequence [waspCliNew "wasp-app" SaaS]),
        -- TestCase
        --   "create-saas-interactive"
        --   (sequence [waspCliNewWithPipedAnswersFails "wasp-app" SaaS])
    ]
  where
    devDbFileInWaspProjectDir =
      generatedAppDirInWaspProjectDir </> dbRootDirInGeneratedAppDir </> devDbFileInDbRootDir

    assertDirectoryExists :: Path' (Rel WaspProjectDir) (Dir d) -> ShellCommandBuilder WaspProjectContext ShellCommand
    assertDirectoryExists dir = return $ "[ -d '" ++ SP.fromRelDir dir ++ "' ]"

    assertDirectoryDoesNotExist :: Path' (Rel WaspProjectDir) (Dir d) -> ShellCommandBuilder WaspProjectContext ShellCommand
    assertDirectoryDoesNotExist dir = return $ "[ ! -d '" ++ SP.fromRelDir dir ++ "' ]"

    assertFileExists :: Path' (Rel WaspProjectDir) (File f) -> ShellCommandBuilder WaspProjectContext ShellCommand
    assertFileExists file = return $ "[ -f '" ++ SP.fromRelFile file ++ "' ]"
