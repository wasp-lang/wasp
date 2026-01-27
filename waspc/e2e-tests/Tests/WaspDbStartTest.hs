module Tests.WaspDbStartTest (waspDbStartTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..))
import Test (Test (..), TestCase (..))
import Test.ShellCommands (createTestWaspProject, inTestWaspProjectDir)
import WaspProject.ShellCommands (setWaspDbToPSQL, waspCliDbStart)

-- FIXME: @waspCliDbStart@ - figure out long lasting processes
waspDbStartTest :: Test
waspDbStartTest =
  Test
    "wasp-db-start"
    [ TestCase
        "fail-outside-project"
        (return [waspCliDbStartFails]),
      TestCase
        "succeed-sqlite-project"
        ( sequence
            [ createTestWaspProject Minimal,
              inTestWaspProjectDir
                [ waspCliDbStart
                ]
            ]
        ),
      TestCase
        "succeed-postgresql-project"
        ( sequence
            [ createTestWaspProject Minimal,
              inTestWaspProjectDir
                [ setWaspDbToPSQL,
                  waspCliDbStart
                ]
            ]
        )
    ]
  where
    waspCliDbStartFails :: ShellCommand
    waspCliDbStartFails = "! wasp-cli db start"
