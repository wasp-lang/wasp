module Tests.SnapshotTests.WaspMigrateSnapshotTest (waspMigrateSnapshotTest) where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import SnapshotTest (SnapshotTest, makeSnapshotTest)
import Steps
  ( appendToPrismaFile,
    createSnapshotWaspProjectFromMinimalStarter,
    inSnapshotWaspProjectDir,
    runCommand,
    waspCliCompile,
    waspCliDbMigrateDev,
  )

waspMigrateSnapshotTest :: SnapshotTest
waspMigrateSnapshotTest =
  makeSnapshotTest "wasp-migrate" $ do
    createSnapshotWaspProjectFromMinimalStarter
    inSnapshotWaspProjectDir $ do
      runCommand waspCliCompile
      appendToPrismaFile taskPrismaModel
      waspCliDbMigrateDev "foo"
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
