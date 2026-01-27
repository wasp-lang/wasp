module Tests.WaspBuildTest (waspBuildTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, inTestWaspProjectDir)
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
              inTestWaspProjectDir [return waspCliBuildFails]
            ]
        ),
      makeTestCase
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
