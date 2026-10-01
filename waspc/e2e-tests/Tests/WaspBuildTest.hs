module Tests.WaspBuildTest (waspBuildTest) where

import ShellCommands
  ( ShellCommand,
    appendToFile,
    assertCommandOutputContains,
    createTestWaspProject,
    inTestWaspProjectDir,
    setWaspDbToPSQL,
    waspCliBuild,
  )
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspBuildTest :: Test
waspBuildTest =
  Test
    "wasp-build"
    [ TestCase
        "fail-outside-project"
        (return [waspCliBuildFails]),
      TestCase
        "fail-sqlite-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir [return waspCliBuildFails]
            ]
        ),
      TestCase
        "succeed-postgresql-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ setWaspDbToPSQL,
                  waspCliBuild,
                  return $ assertDirectoryExists ".wasp",
                  return $ assertDirectoryExists "node_modules"
                ]
            ]
        ),
      TestCase
        "fail-on-user-code-type-error"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ setWaspDbToPSQL,
                  appendToFile "src/MainPage.tsx" "const shouldBeNumber: number = 'wrong'",
                  assertCommandOutputContains
                    (return waspCliBuildFails)
                    "User code type-check failed with exit code:"
                ]
            ]
        )
    ]
  where
    waspCliBuildFails :: ShellCommand
    waspCliBuildFails = "! $WASP_CLI_CMD build"

    assertDirectoryExists :: FilePath -> ShellCommand
    assertDirectoryExists dirFilePath = "[ -d '" ++ dirFilePath ++ "' ]"
