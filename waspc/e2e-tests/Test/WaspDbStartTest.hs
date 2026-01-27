module Test.WaspDbStartTest (waspDbStartTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, withInTestWaspProjectDir)
import WaspProject.ShellCommands (setWaspDbToPSQL, waspCliDbStart)

-- FIXME: @waspCliDbStart@ - figure out long lasting processes
waspDbStartTest :: Test
waspDbStartTest =
  makeTest
    "wasp-db-start"
    [ makeTestCase
        "Should fail outside of a Wasp project"
        (return [waspCliDbStartFails]),
      makeTestCase
        "Should exit early and successfully inside of a SQLite Wasp project"
        ( sequence
            [ createTestWaspProject Minimal,
              withInTestWaspProjectDir [waspCliDbStart]
            ]
        ),
      makeTestCase
        "Should succeed inside of a Postgresql Wasp project"
        ( sequence
            [ createTestWaspProject Minimal,
              withInTestWaspProjectDir
                [ setWaspDbToPSQL,
                  waspCliDbStart
                ]
            ]
        )
    ]
  where
    waspCliDbStartFails :: ShellCommand
    waspCliDbStartFails = "! wasp-cli db start"
