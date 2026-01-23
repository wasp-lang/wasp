module Test.WaspStudioTest (waspStudioTest) where

import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createE2eWaspProject, withInE2eWaspProjectDir)
import WaspProject.ShellCommands (waspCliStudio)

-- | NOTE: Once it evolves it will probably have it's own
-- playwright tests inside of the TS package.
-- So we will tests just that the command works.
-- FIXME: @waspCliStudio@ - figure out long lasting processes
waspStudioTest :: Test
waspStudioTest =
  makeTest
    "wasp-studio"
    [ makeTestCase
        "Should fail outside of a Wasp project"
        waspCliStudioFails,
      makeTestCase
        "Setup: Create Wasp project from minimal starter"
        (createE2eWaspProject Minimal),
      makeTestCase
        "Should succeed inside of a uncompiled Wasp project"
        (withInE2eWaspProjectDir [waspCliStudio])
    ]
  where
    waspCliStudioFails :: ShellCommandBuilder context ShellCommand
    waspCliStudioFails = return "! wasp-cli studio"
