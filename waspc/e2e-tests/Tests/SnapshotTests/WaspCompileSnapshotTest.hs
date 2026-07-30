module Tests.SnapshotTests.WaspCompileSnapshotTest (waspCompileSnapshotTest) where

import SharedActions
  ( createWaspProject,
    inWaspProjectDir,
    runCommand,
    waspCliCompile,
  )
import SnapshotTest (SnapshotTest, makeSnapshotTest)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspCompileSnapshotTest :: SnapshotTest
waspCompileSnapshotTest =
  makeSnapshotTest "wasp-compile" $ do
    createWaspProject minimalStarterTemplate
    inWaspProjectDir $
      runCommand waspCliCompile
