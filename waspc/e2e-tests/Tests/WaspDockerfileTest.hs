module Tests.WaspDockerfileTest (waspDockerfileTest) where

import ShellCommands (ShellCommand, createTestWaspProject, inTestWaspProjectDir, waspCliDockerfile)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

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
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliDockerfile
                ]
            ]
        )
    ]
  where
    waspCliDockerfileFails :: ShellCommand
    waspCliDockerfileFails = "! wasp-cli dockerfile"
