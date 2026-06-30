module Tests.SnapshotTests.WaspCompileSnapshotTest (waspCompileSnapshotTest) where

import SnapshotTest (SnapshotTest, makeSnapshotTest)
import Steps
  ( createWaspProject,
    inWaspProjectDir,
    runCommand,
    waspCliCompile,
  )
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspCompileSnapshotTest :: SnapshotTest
waspCompileSnapshotTest =
  makeSnapshotTest "wasp-compile" $ do
    createWaspProject minimalStarterTemplate
    inWaspProjectDir $
      runCommand waspCliCompile
