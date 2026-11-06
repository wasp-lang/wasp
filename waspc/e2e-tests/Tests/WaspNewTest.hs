module Tests.WaspNewTest (waspNewTest) where

import ShellCommands (ShellCommand, assertCommandOutputContainsAll, waspCliNew, waspCliNewWithPipedAnswersFails)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (basicStarterTemplate, minimalStarterTemplate)

waspNewTest :: Test
waspNewTest =
  Test
    "wasp-new"
    [ TestCase
        "create-minimal"
        ( sequence
            [ waspCliNew "wasp-app" minimalStarterTemplate,
              return $ assertDirectoryDoesNotExist "wasp-app/migrations"
            ]
        ),
      TestCase
        "create-minimal-with-piped-answers-fails"
        (sequence [waspCliNewWithPipedAnswersFails "wasp-app" minimalStarterTemplate]),
      TestCase
        "create-basic"
        ( sequence
            [ waspCliNew "wasp-app" basicStarterTemplate,
              return $ assertDirectoryExists "wasp-app/migrations",
              return $ assertFileExists "wasp-app/.wasp/out/db/dev.db"
            ]
        ),
      TestCase
        "create-basic-leaves-database-from-environment-alone"
        ( sequence
            [ assertCommandOutputContainsAll
                (("DATABASE_URL=file:./unrelated.db " ++) <$> waspCliNew "wasp-app" basicStarterTemplate)
                ["Migration skipped", "wasp db migrate-dev"],
              return $ assertDirectoryDoesNotExist "wasp-app/migrations"
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
    assertDirectoryExists :: FilePath -> ShellCommand
    assertDirectoryExists dirFilePath = "[ -d '" ++ dirFilePath ++ "' ]"

    assertDirectoryDoesNotExist :: FilePath -> ShellCommand
    assertDirectoryDoesNotExist dirFilePath = "[ ! -d '" ++ dirFilePath ++ "' ]"

    assertFileExists :: FilePath -> ShellCommand
    assertFileExists filePath = "[ -f '" ++ filePath ++ "' ]"
