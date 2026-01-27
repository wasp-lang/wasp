module Test.WaspDockerfileTest (waspDockerfileTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, withInTestWaspProjectDir)
import WaspProject.ShellCommands (waspCliDockerfile)

-- TODO: Test `wasp dockerfile` content.
waspDockerfileTest :: Test
waspDockerfileTest =
  makeTest
    "wasp-dockerfile"
    [ makeTestCase
        "Should fail outside of a Wasp project"
        (return [waspCliDockerfileFails]),
      makeTestCase
        "Should succeed inside of a Wasp project"
        ( sequence
            [ createTestWaspProject Minimal,
              withInTestWaspProjectDir [waspCliDockerfile]
            ]
        )
    ]
  where
    waspCliDockerfileFails :: ShellCommand
    waspCliDockerfileFails = "! wasp-cli dockerfile"
