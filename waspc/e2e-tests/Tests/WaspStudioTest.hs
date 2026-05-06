module Tests.WaspStudioTest (waspStudioTest) where

import ShellCommands (ShellCommand, createTestWaspProject, inTestWaspProjectDir, waspCliStudio)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- | NOTE: Once it evolves it will probably have it's own
-- playwright tests inside of the TS package.
-- So we will tests just that the command works.
-- FIXME: @waspCliStudio@ - figure out long lasting processes
waspStudioTest :: Test
waspStudioTest =
  Test
    "wasp-studio"
    [ TestCase
        "fail-outside-project"
        (return [waspCliStudioFails]),
      TestCase
        "succeed-uncompiled-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliStudio
                ]
            ]
        )
    ]
  where
    waspCliStudioFails :: ShellCommand
    waspCliStudioFails = "! wasp-cli studio"
