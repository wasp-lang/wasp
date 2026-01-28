module Tests.WaspDbStartTest (waspDbStartTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..), createTestWaspProject, inTestWaspProjectDir, setWaspDbToPSQL, waspCliDbStart)
import Test (Test (..), TestCase (..))

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
