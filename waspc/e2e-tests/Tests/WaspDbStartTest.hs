module Tests.WaspDbStartTest (waspDbStartTest) where

import Command (Command)
import SharedActions (createWaspProject, inWaspProjectDir, runCommand, runCommandExpectingFailure, setWaspDbToPSQL, waspCli)
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

waspCliDbStart :: Command
waspCliDbStart = waspCli ["db", "start"]
