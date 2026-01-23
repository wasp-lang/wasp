module Test.WaspBuildTest (waspBuildTest) where

import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createE2eWaspProject, withInE2eWaspProjectDir)
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
        (createE2eWaspProject Minimal),
      makeTestCase
        "Should fail inside of a SQLite Wasp project"
        (withInE2eWaspProjectDir [waspCliBuildFails]),
      makeTestCase
        "Setup: Modify Wasp project to use Postgresql"
        (withInE2eWaspProjectDir [setWaspDbToPSQL]),
      makeTestCase
        "Should succeed inside of a Postgresql Wasp project"
        (withInE2eWaspProjectDir [waspCliBuild]),
      -- `wasp build` should also compile the project.
      makeTestCase
        "Assert `.wasp` directory exists"
        (withInE2eWaspProjectDir [assertDirectoryExists ".wasp"]),
      makeTestCase
        "Assert `node_modules` directory exists"
        (withInE2eWaspProjectDir [assertDirectoryExists "node_modules"])
    ]
  where
    waspCliBuildFails :: ShellCommandBuilder context ShellCommand
    waspCliBuildFails = return "! wasp-cli build"

    assertDirectoryExists :: FilePath -> ShellCommandBuilder context ShellCommand
    assertDirectoryExists dirFilePath = return $ "[ -d '" ++ dirFilePath ++ "' ]"
