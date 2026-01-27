module Test.WaspBuildStartTest (waspBuildStartTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, withInTestWaspProjectDir)
import WaspProject.ShellCommands (setWaspDbToPSQL, waspCliBuild, waspCliBuildStart)

-- FIXME: @waspCliBuildStart@ - figure out long lasting processes
waspBuildStartTest :: Test
waspBuildStartTest =
  makeTest
    "wasp-build-start"
    [ makeTestCase
        "Should fail outside of a Wasp project"
        (return [waspCliBuildStartFails]),
      makeTestCase
        "Should fail inside of a unbuilt Wasp project"
        ( sequence
            [ createTestWaspProject Minimal,
              withInTestWaspProjectDir
                [ setWaspDbToPSQL,
                  return waspCliBuildStartFails
                ]
            ]
        ),
      makeTestCase
        "Should succeed inside of a built Wasp project"
        ( sequence
            [ createTestWaspProject Minimal,
              withInTestWaspProjectDir
                [ setWaspDbToPSQL,
                  waspCliBuild,
                  waspCliBuildStart "-s DATABASE_URL=none"
                ]
            ]
        )
    ]
  where
    waspCliBuildStartFails :: ShellCommand
    waspCliBuildStartFails = "! wasp-cli build start"
