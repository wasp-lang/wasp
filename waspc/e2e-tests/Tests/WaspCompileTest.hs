module Tests.WaspCompileTest (waspCompileTest) where

import Steps
  ( assertDirExists,
    createTestWaspProject,
    inTestWaspProjectDir,
    runCommand,
    runCommandExpectingFailure,
    waspCliCompile,
  )
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspCompileTest :: Test
waspCompileTest =
  Test
    "wasp-compile"
    [ TestCase "fail-outside-project" $
        runCommandExpectingFailure waspCliCompile,
      TestCase "succeed-uncompiled-project" $ do
        createTestWaspProject minimalStarterTemplate
        inTestWaspProjectDir $ do
          runCommand waspCliCompile
          assertDirExists ".wasp"
          assertDirExists "node_modules",
      TestCase "succeed-compiled-project" $ do
        createTestWaspProject minimalStarterTemplate
        inTestWaspProjectDir $ do
          runCommand waspCliCompile
          runCommand waspCliCompile
          assertDirExists ".wasp"
          assertDirExists "node_modules"
    ]
