module Test.WaspBuildStartTest (waspBuildStartTest) where

import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, withInTestWaspProjectDir)
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
        (createTestWaspProject Minimal),
      makeTestCase
        "Setup: Modify Wasp project to use Postgresql"
        (withInTestWaspProjectDir [setWaspDbToPSQL]),
      makeTestCase
        "Should fail inside of a unbuilt Wasp project"
        (withInTestWaspProjectDir [waspCliBuildStart "-s DATABASE_URL=none"]),
      makeTestCase
        "Setup: Build the Wasp project"
        (withInTestWaspProjectDir [waspCliBuild]),
      makeTestCase
        "Should succeed inside of a built Wasp project"
        (withInTestWaspProjectDir [waspCliBuildStart "-s DATABASE_URL=none"])
    ]
  where
    waspCliBuildStartFails :: ShellCommandBuilder context ShellCommand
    waspCliBuildStartFails = return "! wasp-cli build start"
