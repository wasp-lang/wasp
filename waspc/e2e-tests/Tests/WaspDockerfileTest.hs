module Tests.WaspDockerfileTest (waspDockerfileTest) where

import Steps (createTestWaspProject, inTestWaspProjectDir, runCommand, runCommandExpectingFailure, waspCliDockerfile)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- TODO: Test `wasp dockerfile` content.
waspDockerfileTest :: Test
waspDockerfileTest =
  Test
    "wasp-dockerfile"
    [ TestCase
        "fail-outside-project"
        [runCommandExpectingFailure waspCliDockerfile],
      TestCase
        "succeed-inside-project"
        [ createTestWaspProject minimalStarterTemplate,
          inTestWaspProjectDir
            [ runCommand waspCliDockerfile
            ]
        ]
    ]
