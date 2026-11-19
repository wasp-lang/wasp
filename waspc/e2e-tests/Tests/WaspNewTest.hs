module Tests.WaspNewTest (waspNewTest) where

import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext,
    assertCommandOutputContains,
    assertCommandOutputContainsAll,
    createTestWaspProject,
    createTestWaspProjectWithPackageManager,
    inTestWaspProjectDir,
    waspCliCompile,
    waspCliNew,
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
import Wasp.Project.PackageManager (PackageManager (Pnpm))

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
        "create-minimal-with-pnpm"
        ( sequence
            [ createTestWaspProjectWithPackageManager Pnpm minimalStarterTemplate,
              inTestWaspProjectDir
                [ return "[ -f pnpm-workspace.yaml ]",
                  return "[ -f pnpm-lock.yaml ]",
                  return "[ ! -f .npmrc ]",
                  return "[ ! -f package-lock.json ]",
                  waspCliCompile
                ]
            ]
        ),
      TestCase
        "create-with-unknown-package-manager-fails"
        ( sequence
            [ assertCommandOutputContains
                (("! " ++) <$> waspCliNewWithPackageManagerName "yarn" "wasp-app" minimalStarterTemplate)
                "Wasp doesn't work with the package manager 'yarn'"
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
    waspCliNewWithPackageManagerName packageManagerName appName starterTemplate =
      (++ " --package-manager " ++ packageManagerName) <$> waspCliNew appName starterTemplate

    devDbFileInWaspProjectDir =
      generatedAppDirInWaspProjectDir </> dbRootDirInGeneratedAppDir </> devDbFileInDbRootDir

    assertDirectoryExists :: Path' (Rel WaspProjectDir) (Dir d) -> ShellCommandBuilder WaspProjectContext ShellCommand
    assertDirectoryExists dir = return $ "[ -d '" ++ SP.fromRelDir dir ++ "' ]"

    assertDirectoryDoesNotExist :: Path' (Rel WaspProjectDir) (Dir d) -> ShellCommandBuilder WaspProjectContext ShellCommand
    assertDirectoryDoesNotExist dir = return $ "[ ! -d '" ++ SP.fromRelDir dir ++ "' ]"

    assertFileExists :: Path' (Rel WaspProjectDir) (File f) -> ShellCommandBuilder WaspProjectContext ShellCommand
    assertFileExists file = return $ "[ -f '" ++ SP.fromRelFile file ++ "' ]"
