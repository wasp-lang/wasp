module Tests.WaspDbStudioTest (waspDbStudioTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, inTestWaspProjectDir)
import WaspProject.ShellCommands (waspCliDbStudio)

-- | NOTE: We don't test feature content since it's prisma feature.
-- FIXME: @waspCliDbStudio@ - figure out long lasting processes
waspDbStudioTest :: Test
waspDbStudioTest =
  makeTest
    "wasp-db-studio"
    [ makeTestCase
        "fail-outside-project"
        (return [waspCliDbStudioFails]),
      makeTestCase
        "succeed-uncompiled-project"
        ( sequence
            [ createTestWaspProject Minimal,
              inTestWaspProjectDir
                [ waspCliDbStudio
                ]
            ]
        )
    ]
  where
    waspCliDbStudioFails :: ShellCommand
    waspCliDbStudioFails = "! wasp-cli db studio"
