module Test.WaspDockerfileTest (waspDockerfileTest) where

import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createE2eWaspProject, withInE2eWaspProjectDir)
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
        (createE2eWaspProject Minimal),
      makeTestCase
        "Should succeed inside of a Wasp project"
        (withInE2eWaspProjectDir [waspCliDockerfile])
    ]
  where
    waspCliDockerfileFails :: ShellCommandBuilder context ShellCommand
    waspCliDockerfileFails = return "! wasp-cli dockerfile"
