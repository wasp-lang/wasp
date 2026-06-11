module Tests.WaspDbStartTest (waspDbStartTest) where

import Steps (createTestWaspProject, inTestWaspProjectDir, runCommand, runCommandExpectingFailure, setWaspDbToPSQL, waspCliDbStart)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- FIXME: @waspCliDbStart@ - figure out long lasting processes
waspDbStartTest :: Test
waspDbStartTest =
  Test
    "wasp-db-start"
    [ TestCase "fail-outside-project" $
        runCommandExpectingFailure waspCliDbStart,
      TestCase "succeed-sqlite-project" $ do
        createTestWaspProject minimalStarterTemplate
        inTestWaspProjectDir $
          runCommand waspCliDbStart,
      TestCase "succeed-postgresql-project" $ do
        createTestWaspProject minimalStarterTemplate
        inTestWaspProjectDir $ do
          setWaspDbToPSQL
          runCommand waspCliDbStart
    ]
