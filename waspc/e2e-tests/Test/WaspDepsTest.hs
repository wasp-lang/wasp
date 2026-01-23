module Test.WaspDepsTest (waspDepsTest) where

import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createE2eWaspProject, withInE2eWaspProjectDir)
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
        (createE2eWaspProject Minimal),
      makeTestCase
        "Should succeed inside of a Wasp project"
        (withInE2eWaspProjectDir [waspCliDeps])
    ]
  where
    waspCliDepsFails :: ShellCommandBuilder context ShellCommand
    waspCliDepsFails = return "! wasp-cli deps"
