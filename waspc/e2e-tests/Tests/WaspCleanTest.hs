module Tests.WaspCleanTest (waspCleanTest) where

import Steps
  ( assertDirDoesNotExist,
    createWaspProject,
    inWaspProjectDir,
    runCommand,
    runCommandExpectingFailure,
    waspCliClean,
    waspCliCompile,
  )
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspCleanTest :: Test
waspCleanTest =
  Test
    "wasp-clean"
    [ TestCase "fail-outside-project" $
        runCommandExpectingFailure waspCliClean,
      TestCase "succeed-uncompiled-project" $ do
        createWaspProject minimalStarterTemplate
        inWaspProjectDir $ do
          runCommand waspCliClean
          assertDirDoesNotExist ".wasp"
          assertDirDoesNotExist "node_modules",
      TestCase "succeed-compiled-project" $ do
        createWaspProject minimalStarterTemplate
        inWaspProjectDir $ do
          runCommand waspCliCompile
          runCommand waspCliClean
          assertDirDoesNotExist ".wasp"
          assertDirDoesNotExist "node_modules"
    ]
