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
        "fail-outside-project"
        (return [waspCliDbStartFails]),
      makeTestCase
        "succeed-sqlite-project"
        ( sequence
            [ createTestWaspProject Minimal,
              withInTestWaspProjectDir
                [ waspCliDbStart
                ]
            ]
        ),
      makeTestCase
        "succeed-postgresql-project"
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
