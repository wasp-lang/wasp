module Tests.WaspDepsTest (waspDepsTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..), createTestWaspProject, inTestWaspProjectDir, waspCliDeps)
import Test (Test (..), TestCase (..))

-- TODO: Test that deps change with installs/uninstalls.
waspDepsTest :: Test
waspDepsTest =
  Test
    "wasp-deps"
    [ TestCase
        "fail-outside-project"
        (return [waspCliDepsFails]),
      TestCase
        "succeed-inside-project"
        ( sequence
            [ createTestWaspProject Minimal,
              inTestWaspProjectDir
                [ waspCliDeps
                ]
            ]
        )
    ]
  where
    waspCliDepsFails :: ShellCommand
    waspCliDepsFails = "! wasp-cli deps"
