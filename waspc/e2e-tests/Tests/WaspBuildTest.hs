module Tests.WaspBuildTest (waspBuildTest) where

import Steps
  ( assertDirExists,
    createWaspProject,
    inWaspProjectDir,
    runCommand,
    runCommandExpectingFailure,
    setWaspDbToPSQL,
    waspCliBuild,
  )
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspBuildTest :: Test
waspBuildTest =
  Test
    "wasp-build"
    [ TestCase "fail-outside-project" $
        runCommandExpectingFailure waspCliBuild,
      TestCase "fail-sqlite-project" $ do
        createWaspProject minimalStarterTemplate
        inWaspProjectDir $
          runCommandExpectingFailure waspCliBuild,
      TestCase "succeed-postgresql-project" $ do
        createWaspProject minimalStarterTemplate
        inWaspProjectDir $ do
          setWaspDbToPSQL
          runCommand waspCliBuild
          assertDirExists ".wasp"
          assertDirExists "node_modules"
    ]
