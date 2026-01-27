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
        "fail-outside-project"
        (return [waspCliBuildFails]),
      makeTestCase
        "fail-sqlite-project"
        ( sequence
            [ createTestWaspProject Minimal,
              withInTestWaspProjectDir [return waspCliBuildFails]
            ]
        ),
      makeTestCase
        "succeed-postgresql-project"
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
