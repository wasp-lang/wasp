module Test.WaspDockerfileTest (waspDockerfileTest) where

import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, withInTestWaspProjectDir)
import WaspProject.ShellCommands (waspCliDockerfile)

-- TODO: Test `wasp dockerfile` content.
waspDockerfileTest :: Test
waspDockerfileTest =
  makeTest
    "wasp-dockerfile"
    [ makeTestCase
        "Should fail outside of a Wasp project"
        waspCliDockerfileFails,
      makeTestCase
        "Setup: Create Wasp project from minimal starter"
        (createTestWaspProject Minimal),
      makeTestCase
        "Should succeed inside of a Wasp project"
        (withInTestWaspProjectDir [waspCliDockerfile])
    ]
  where
    waspCliDockerfileFails :: ShellCommandBuilder context ShellCommand
    waspCliDockerfileFails = return "! wasp-cli dockerfile"
