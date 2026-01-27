module Test.WaspBuildTest (waspBuildTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, withInTestWaspProjectDir)
import WaspProject.ShellCommands (setWaspDbToPSQL, waspCliBuild)

waspBuildTest :: Test
waspBuildTest =
  makeTest
    "wasp-build"
    [ makeTestCase
        "Should fail outside of a Wasp project"
        (return [waspCliBuildFails]),
      makeTestCase
        "Should fail inside of a SQLite Wasp project"
        ( sequence
            [ createTestWaspProject Minimal,
              withInTestWaspProjectDir [return waspCliBuildFails]
            ]
        ),
      -- `wasp build` should also compile the project.
      makeTestCase
        "Should succeed inside of a Postgresql Wasp project"
        ( sequence
            [ createTestWaspProject Minimal,
              withInTestWaspProjectDir
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
