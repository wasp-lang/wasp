module Tests.SnapshotTests.WaspMigrateSnapshotTest (waspMigrateSnapshotTest) where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import ShellCommands
  ( appendToPrismaFile,
    createSnapshotWaspProjectFromMinimalStarter,
    waspCliCompile,
    waspCliDbMigrateDev,
    withInSnapshotWaspProjectDir,
  )
import SnapshotTest (SnapshotTest, makeSnapshotTest)

waspMigrateSnapshotTest :: SnapshotTest
waspMigrateSnapshotTest =
  makeSnapshotTest
    "wasp-migrate"
    [ createSnapshotWaspProjectFromMinimalStarter,
      withInSnapshotWaspProjectDir
        [ waspCliCompile,
          appendToPrismaFile taskPrismaModel,
          waspCliDbMigrateDev "foo"
        ]
    ]
  where
    taskPrismaModel :: T.Text
    taskPrismaModel =
      [trimming|
        model Task {
          id          Int     @id @default(autoincrement())
          description String
          isDone      Boolean @default(false)
        }
      |]
