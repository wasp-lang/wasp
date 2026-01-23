module Test.WaspDepsTest (waspDepsTest) where

import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, withInTestWaspProjectDir)
import WaspProject.ShellCommands (waspCliDeps)

-- TODO: Test that deps change with installs/uninstalls.
waspDepsTest :: Test
waspDepsTest =
  makeTest
    "wasp-deps"
    [ makeTestCase
        "Should fail outside of a Wasp project"
        waspCliDepsFails,
      makeTestCase
        "Setup: Create Wasp project from minimal starter"
        (createTestWaspProject Minimal),
      makeTestCase
        "Should succeed inside of a Wasp project"
        (withInTestWaspProjectDir [waspCliDeps])
    ]
  where
    waspCliDepsFails :: ShellCommandBuilder context ShellCommand
    waspCliDepsFails = return "! wasp-cli deps"
