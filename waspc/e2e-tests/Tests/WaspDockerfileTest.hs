module Tests.WaspDockerfileTest (waspDockerfileTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, inTestWaspProjectDir)
import WaspProject.ShellCommands (waspCliDockerfile)

-- TODO: Test `wasp dockerfile` content.
waspDockerfileTest :: Test
waspDockerfileTest =
  makeTest
    "wasp-dockerfile"
    [ makeTestCase
        "fail-outside-project"
        (return [waspCliDockerfileFails]),
      makeTestCase
        "succeed-inside-project"
        ( sequence
            [ createTestWaspProject Minimal,
              inTestWaspProjectDir
                [ waspCliDockerfile
                ]
            ]
        )
    ]
  where
    waspCliDockerfileFails :: ShellCommand
    waspCliDockerfileFails = "! wasp-cli dockerfile"
