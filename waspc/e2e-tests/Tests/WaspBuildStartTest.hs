module Tests.WaspBuildStartTest (waspBuildStartTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, inTestWaspProjectDir)
import WaspProject.ShellCommands (setWaspDbToPSQL, waspCliBuild, waspCliBuildStart)

-- FIXME: @waspCliBuildStart@ - figure out long lasting processes
waspBuildStartTest :: Test
waspBuildStartTest =
  makeTest
    "wasp-build-start"
    [ makeTestCase
        "fail-outside-project"
        (return [waspCliBuildStartFails]),
      makeTestCase
        "fail-unbuilt-project"
        ( sequence
            [ createTestWaspProject Minimal,
              inTestWaspProjectDir
                [ setWaspDbToPSQL,
                  return waspCliBuildStartFails
                ]
            ]
        ),
      makeTestCase
        "succeed-built-project"
        ( sequence
            [ createTestWaspProject Minimal,
              inTestWaspProjectDir
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
