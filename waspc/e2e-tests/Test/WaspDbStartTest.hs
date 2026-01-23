module Test.WaspDbStartTest (waspDbStartTest) where

import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createE2eWaspProject, withInE2eWaspProjectDir)
import WaspProject.ShellCommands (setWaspDbToPSQL, waspCliDbStart)

-- FIXME: @waspCliDbStart@ - figure out long lasting processes
waspDbStartTest :: Test
waspDbStartTest =
  makeTest
    "wasp-db-start"
    [ makeTestCase
        "Should fail outside of a Wasp project"
        waspCliDbStartFails,
      makeTestCase
        "Setup: Create Wasp project from minimal starter"
        (createE2eWaspProject Minimal),
      makeTestCase
        "Should exit early and successfully inside of a SQLite Wasp project"
        (withInE2eWaspProjectDir [waspCliDbStart]),
      makeTestCase
        "Setup: Modify Wasp project to use Postgresql"
        (withInE2eWaspProjectDir [setWaspDbToPSQL]),
      makeTestCase
        "Should succeed inside of a Wasp project"
        (withInE2eWaspProjectDir [waspCliDbStart])
    ]
  where
    waspCliDbStartFails :: ShellCommandBuilder context ShellCommand
    waspCliDbStartFails = return "! wasp-cli db start"
