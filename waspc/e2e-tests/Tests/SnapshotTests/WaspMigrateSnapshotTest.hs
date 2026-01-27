module Tests.SnapshotTests.WaspMigrateSnapshotTest (waspMigrateSnapshotTest) where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import SnapshotTest (SnapshotTest, makeSnapshotTest)
import SnapshotTest.ShellCommands
  ( createSnapshotWaspProjectFromMinimalStarter,
    withInSnapshotWaspProjectDir,
  )
import WaspProject.ShellCommands
  ( appendToPrismaFile,
    waspCliCompile,
    waspCliDbMigrateDev,
  )

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
