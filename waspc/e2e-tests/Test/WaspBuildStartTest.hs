module Test.WaspBuildStartTest (waspBuildStartTest) where

import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createE2eWaspProject, withInE2eWaspProjectDir)
import WaspProject.ShellCommands (setWaspDbToPSQL, waspCliBuild, waspCliBuildStart)

-- FIXME: @waspCliBuildStart@ - figure out long lasting processes
waspBuildStartTest :: Test
waspBuildStartTest =
  makeTest
    "wasp-build-start"
    [ makeTestCase
        "Should fail outside of a Wasp project"
        waspCliBuildStartFails,
      makeTestCase
        "Setup: Create Wasp project from minimal starter"
        (createE2eWaspProject Minimal),
      makeTestCase
        "Setup: Modify Wasp project to use Postgresql"
        (withInE2eWaspProjectDir [setWaspDbToPSQL]),
      makeTestCase
        "Should fail inside of a unbuilt Wasp project"
        (withInE2eWaspProjectDir [waspCliBuildStart "-s DATABASE_URL=none"]),
      makeTestCase
        "Setup: Build the Wasp project"
        (withInE2eWaspProjectDir [waspCliBuild]),
      makeTestCase
        "Should succeed inside of a built Wasp project"
        (withInE2eWaspProjectDir [waspCliBuildStart "-s DATABASE_URL=none"])
    ]
  where
    waspCliBuildStartFails :: ShellCommandBuilder context ShellCommand
    waspCliBuildStartFails = return "! wasp-cli build start"
