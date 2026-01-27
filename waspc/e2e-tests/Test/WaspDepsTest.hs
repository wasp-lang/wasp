module Test.WaspDepsTest (waspDepsTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, withInTestWaspProjectDir)
import WaspProject.ShellCommands (waspCliDeps)

-- TODO: Test that deps change with installs/uninstalls.
waspDepsTest :: Test
waspDepsTest =
  makeTest
    "wasp-deps"
    [ makeTestCase
        "Should fail outside of a Wasp project"
        (return [waspCliDepsFails]),
      makeTestCase
        "Should succeed inside of a Wasp project"
        ( sequence
            [ createTestWaspProject Minimal,
              withInTestWaspProjectDir [waspCliDeps]
            ]
        )
    ]
  where
    waspCliDepsFails :: ShellCommand
    waspCliDepsFails = "! wasp-cli deps"
