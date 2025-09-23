module Tests.WaspMigrateSnapshotTest (waspMigrateSnapshotTest) where

import SnapshotTest (SnapshotTest, makeSnapshotTest)
import SnapshotTest.ShellCommands
  ( createSnapshotWaspApp,
    withInSnapshotWaspAppDir,
  )
import WaspApp.ShellCommands
  ( appendToPrismaFile,
    waspCliCompile,
    waspCliMigrate,
  )

waspMigrateSnapshotTest :: SnapshotTest
waspMigrateSnapshotTest =
  makeSnapshotTest
    "wasp-migrate"
    [ createSnapshotWaspApp,
      withInSnapshotWaspAppDir
        [ waspCliCompile,
          appendToPrismaFile taskPrismaModel,
          waspCliMigrate "foo"
        ]
    ]
  where
    taskPrismaModel =
      unlines
        [ "model Task {",
          "  id          Int     @id @default(autoincrement())",
          "  description String",
          "  isDone      Boolean @default(false)",
          "}"
        ]
