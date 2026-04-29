module Tests.WaspDepsTest (waspDepsTest) where

import ShellCommands (ShellCommand, createTestWaspProject, inTestWaspProjectDir, waspCliDeps)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

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
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliDeps
                ]
            ]
        )
    ]
  where
    waspCliDepsFails :: ShellCommand
    waspCliDepsFails = "! wasp-cli deps"
