module Tests.WaspStartTest (waspStartTest) where

import Steps (assertDirExists, createTestWaspProject, inTestWaspProjectDir, runCommand, runCommandExpectingFailure, waspCliCompile, waspCliStart)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- FIXME: @waspCliStart@ - figure out long lasting processes
waspStartTest :: Test
waspStartTest =
  Test
    "wasp-start"
    [ TestCase "fail-outside-project" $
        runCommandExpectingFailure waspCliStart,
      TestCase "succeed-uncompiled-project" $ do
        createTestWaspProject minimalStarterTemplate
        inTestWaspProjectDir $ do
          runCommand waspCliStart
          assertDirExists ".wasp"
          assertDirExists "node_modules",
      TestCase "succeed-compiled-project" $ do
        createTestWaspProject minimalStarterTemplate
        inTestWaspProjectDir $ do
          runCommand waspCliCompile
          runCommand waspCliStart
    ]
