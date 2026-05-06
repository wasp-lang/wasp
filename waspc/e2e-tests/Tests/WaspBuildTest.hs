module Tests.WaspBuildTest (waspBuildTest) where

import ShellCommands (ShellCommand, createTestWaspProject, inTestWaspProjectDir, setWaspDbToPSQL, waspCliBuild)
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
        )
    ]
  where
    waspCliBuildFails :: ShellCommand
    waspCliBuildFails = "! wasp-cli build"

    assertDirectoryExists :: FilePath -> ShellCommand
    assertDirectoryExists dirFilePath = "[ -d '" ++ dirFilePath ++ "' ]"
