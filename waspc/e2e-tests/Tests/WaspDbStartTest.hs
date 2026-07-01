module Tests.WaspDbStartTest (waspDbStartTest) where

import Steps (createWaspProject, inWaspProjectDir, runCommand, runCommandExpectingFailure, setWaspDbToPSQL, waspCliDbStart)
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
        createWaspProject minimalStarterTemplate
        inWaspProjectDir $
          runCommand waspCliDbStart,
      TestCase "succeed-postgresql-project" $ do
        createWaspProject minimalStarterTemplate
        inWaspProjectDir $ do
          setWaspDbToPSQL
          runCommand waspCliDbStart
    ]
