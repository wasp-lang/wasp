module Tests.WaspDepsTest (waspDepsTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, inTestWaspProjectDir)
import WaspProject.ShellCommands (waspCliDeps)

-- TODO: Test that deps change with installs/uninstalls.
waspDepsTest :: Test
waspDepsTest =
  makeTest
    "wasp-deps"
    [ makeTestCase
        "fail-outside-project"
        (return [waspCliDepsFails]),
      makeTestCase
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
