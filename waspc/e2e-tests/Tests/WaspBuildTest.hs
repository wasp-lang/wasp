module Tests.WaspBuildTest (waspBuildTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..))
import Test (Test (..), TestCase (..))
import Test.ShellCommands (createTestWaspProject, inTestWaspProjectDir)
import WaspProject.ShellCommands (setWaspDbToPSQL, waspCliBuild)

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
            [ createTestWaspProject Minimal,
              inTestWaspProjectDir [return waspCliBuildFails]
            ]
        ),
      TestCase
        "succeed-postgresql-project"
        ( sequence
            [ createTestWaspProject Minimal,
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
