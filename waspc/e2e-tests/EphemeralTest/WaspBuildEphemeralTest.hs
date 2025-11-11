module EphemeralTest.WaspBuildEphemeralTest (waspBuildEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import EphemeralTest.ShellCommands (createEphemeralWaspProject, withInEphemeralWaspProjectDir)
import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate (..))
import WaspProject.ShellCommands (setWaspDbToPSQL, waspCliBuild)

waspBuildEphemeralTest :: EphemeralTest
waspBuildEphemeralTest =
  makeEphemeralTest
    "wasp-build"
    [ makeEphemeralTestCase
        "Should fail outside of a Wasp project"
        waspCliBuildFails,
      makeEphemeralTestCase
        "Setup: Create Wasp project from minimal starter"
        (createEphemeralWaspProject Minimal),
      makeEphemeralTestCase
        "Should fail inside of a SQLite Wasp project"
        (withInEphemeralWaspProjectDir [waspCliBuildFails]),
      makeEphemeralTestCase
        "Setup: Modify Wasp project to use Postgresql"
        (withInEphemeralWaspProjectDir [setWaspDbToPSQL]),
      makeEphemeralTestCase
        "Should succeed inside of a Postgresql Wasp project"
        (withInEphemeralWaspProjectDir [waspCliBuild]),
      -- `wasp build` should compile the project
      makeEphemeralTestCase
        "Assert `.wasp` directory exists"
        (withInEphemeralWaspProjectDir [assertDirectoryExists ".wasp"]),
      makeEphemeralTestCase
        "Assert `node_modules` directory exists"
        (withInEphemeralWaspProjectDir [assertDirectoryExists "node_modules"])
    ]
  where
    waspCliBuildFails :: ShellCommandBuilder context ShellCommand
    waspCliBuildFails = return "! wasp-cli build"

    assertDirectoryExists :: FilePath -> ShellCommandBuilder context ShellCommand
    assertDirectoryExists dirFilePath = return $ "[ -d '" ++ dirFilePath ++ "' ]"
