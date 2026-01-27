module Test.WaspDbStudioTest (waspDbStudioTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, withInTestWaspProjectDir)
import WaspProject.ShellCommands (waspCliDbStudio)

-- | NOTE: We don't test feature content since it's prisma feature.
-- FIXME: @waspCliDbStudio@ - figure out long lasting processes
waspDbStudioTest :: Test
waspDbStudioTest =
  makeTest
    "wasp-db-studio"
    [ makeTestCase
        "Should fail outside of a Wasp project"
        (return [waspCliDbStudioFails]),
      makeTestCase
        "Should succeed inside of a uncompiled Wasp project"
        ( sequence
            [ createTestWaspProject Minimal,
              withInTestWaspProjectDir [waspCliDbStudio]
            ]
        )
    ]
  where
    waspCliDbStudioFails :: ShellCommand
    waspCliDbStudioFails = "! wasp-cli db studio"
