module Tests.WaspStartTest (waspStartTest) where

import Steps (assertDirExists, createWaspProject, inWaspProjectDir, runCommand, runCommandExpectingFailure, waspCliCompile, waspCliStart)
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
        createWaspProject minimalStarterTemplate
        inWaspProjectDir $ do
          runCommand waspCliStart
          assertDirExists ".wasp"
          assertDirExists "node_modules",
      TestCase "succeed-compiled-project" $ do
        createWaspProject minimalStarterTemplate
        inWaspProjectDir $ do
          runCommand waspCliCompile
          runCommand waspCliStart
    ]
