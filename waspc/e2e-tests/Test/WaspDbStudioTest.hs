module Test.WaspDbStudioTest (waspDbStudioTest) where

import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createE2eWaspProject, withInE2eWaspProjectDir)
import WaspProject.ShellCommands (waspCliDbStudio)

-- | NOTE: We don't test feature content since it's prisma feature.
-- FIXME: @waspCliDbStudio@ - figure out long lasting processes
waspDbStudioTest :: Test
waspDbStudioTest =
  makeTest
    "wasp-db-studio"
    [ makeTestCase
        "Should fail outside of a Wasp project"
        waspCliDbStudioFails,
      makeTestCase
        "Setup: Create Wasp project from minimal starter"
        (createE2eWaspProject Minimal),
      makeTestCase
        "Should succeed inside of a uncompiled Wasp project"
        (withInE2eWaspProjectDir [waspCliDbStudio])
    ]
  where
    waspCliDbStudioFails :: ShellCommandBuilder context ShellCommand
    waspCliDbStudioFails = return "! wasp-cli db studio"
