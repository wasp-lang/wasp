module Tests.WaspMigrateSnapshotTest (waspMigrateSnapshotTest) where

import SnapshotTest (SnapshotTest, makeSnapshotTest)
import SnapshotTest.TestCommands
  ( createSnapshotWaspProjectFromMinimalStarter,
    withInSnapshotWaspProjectDir,
  )
import WaspProject.TestCommands
  ( appendToPrismaFile,
    waspCliCompile,
    waspCliMigrate,
  )

waspMigrateSnapshotTest :: SnapshotTest
waspMigrateSnapshotTest =
  makeSnapshotTest
    "wasp-migrate"
    [ createSnapshotWaspProjectFromMinimalStarter,
      withInSnapshotWaspProjectDir
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
