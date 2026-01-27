module Test.WaspStudioTest (waspStudioTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, withInTestWaspProjectDir)
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
        (return [waspCliStudioFails]),
      makeTestCase
        "Should succeed inside of a uncompiled Wasp project"
        ( sequence
            [ createTestWaspProject Minimal,
              withInTestWaspProjectDir [waspCliStudio]
            ]
        )
    ]
  where
    waspCliStudioFails :: ShellCommand
    waspCliStudioFails = "! wasp-cli studio"
