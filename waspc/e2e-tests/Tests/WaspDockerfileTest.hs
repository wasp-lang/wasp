module Tests.WaspDockerfileTest (waspDockerfileTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..))
import Test (Test (..), TestCase (..))
import Test.ShellCommands (createTestWaspProject, inTestWaspProjectDir)
import WaspProject.ShellCommands (waspCliDockerfile)

-- TODO: Test `wasp dockerfile` content.
waspDockerfileTest :: Test
waspDockerfileTest =
  Test
    "wasp-dockerfile"
    [ TestCase
        "fail-outside-project"
        (return [waspCliDockerfileFails]),
      TestCase
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
