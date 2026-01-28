module Tests.WaspDockerfileTest (waspDockerfileTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..), createTestWaspProject, inTestWaspProjectDir, waspCliDockerfile)
import Test (Test (..), TestCase (..))

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
