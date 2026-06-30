module Tests.WaspInfoTest (waspInfoTest) where

import Steps (createWaspProject, inWaspProjectDir, runCommand, runCommandExpectingFailure, waspCliInfo)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- TODO: Test `wasp info` values change properly:
-- name, database, project dir size, last compile.
waspInfoTest :: Test
waspInfoTest =
  Test
    "wasp-info"
    [ TestCase "fail-outside-project" $
        runCommandExpectingFailure waspCliInfo,
      TestCase "succeed-inside-project" $ do
        createWaspProject minimalStarterTemplate
        inWaspProjectDir $
          runCommand waspCliInfo
    ]
