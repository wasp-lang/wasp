module Test.WaspBuildTest (waspBuildTest) where

import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, withInTestWaspProjectDir)
import WaspProject.ShellCommands (setWaspDbToPSQL, waspCliBuild)

waspBuildTest :: Test
waspBuildTest =
  makeTest
    "wasp-build"
    [ makeTestCase
        "Should fail outside of a Wasp project"
        waspCliBuildFails,
      makeTestCase
        "Setup: Create Wasp project from minimal starter"
        (createTestWaspProject Minimal),
      makeTestCase
        "Should fail inside of a SQLite Wasp project"
        (withInTestWaspProjectDir [waspCliBuildFails]),
      makeTestCase
        "Setup: Modify Wasp project to use Postgresql"
        (withInTestWaspProjectDir [setWaspDbToPSQL]),
      makeTestCase
        "Should succeed inside of a Postgresql Wasp project"
        (withInTestWaspProjectDir [waspCliBuild]),
      -- `wasp build` should also compile the project.
      makeTestCase
        "Assert `.wasp` directory exists"
        (withInTestWaspProjectDir [assertDirectoryExists ".wasp"]),
      makeTestCase
        "Assert `node_modules` directory exists"
        (withInTestWaspProjectDir [assertDirectoryExists "node_modules"])
    ]
  where
    waspCliBuildFails :: ShellCommandBuilder context ShellCommand
    waspCliBuildFails = return "! wasp-cli build"

    assertDirectoryExists :: FilePath -> ShellCommandBuilder context ShellCommand
    assertDirectoryExists dirFilePath = return $ "[ -d '" ++ dirFilePath ++ "' ]"
