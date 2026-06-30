module Tests.WaspBuildStartTest (waspBuildStartTest) where

import Steps (createWaspProject, inWaspProjectDir, runCommand, runCommandExpectingFailure, setWaspDbToPSQL, waspCliBuild, waspCliBuildStart)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- FIXME: @waspCliBuildStart@ - figure out long lasting processes
waspBuildStartTest :: Test
waspBuildStartTest =
  Test
    "wasp-build-start"
    [ TestCase "fail-outside-project" $
        runCommandExpectingFailure $
          waspCliBuildStart [],
      TestCase "fail-unbuilt-project" $ do
        createWaspProject minimalStarterTemplate
        inWaspProjectDir $ do
          setWaspDbToPSQL
          runCommandExpectingFailure $ waspCliBuildStart [],
      TestCase "succeed-built-project" $ do
        createWaspProject minimalStarterTemplate
        inWaspProjectDir $ do
          setWaspDbToPSQL
          runCommand waspCliBuild
          runCommand $ waspCliBuildStart ["-s", "DATABASE_URL=none"]
    ]
