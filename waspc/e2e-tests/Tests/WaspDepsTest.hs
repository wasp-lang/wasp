module Tests.WaspDepsTest (waspDepsTest) where

import Steps (createTestWaspProject, inTestWaspProjectDir, runCommand, runCommandExpectingFailure, waspCliDeps)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- TODO: Test that deps change with installs/uninstalls.
waspDepsTest :: Test
waspDepsTest =
  Test
    "wasp-deps"
    [ TestCase "fail-outside-project" $
        runCommandExpectingFailure waspCliDeps,
      TestCase "succeed-inside-project" $ do
        createTestWaspProject minimalStarterTemplate
        inTestWaspProjectDir $
          runCommand waspCliDeps
    ]
