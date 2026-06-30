module Tests.SnapshotTests.WaspMigrateSnapshotTest (waspMigrateSnapshotTest) where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import SnapshotTest (SnapshotTest, makeSnapshotTest)
import Steps
  ( appendToPrismaFile,
    createWaspProject,
    inWaspProjectDir,
    runCommand,
    waspCliCompile,
    waspCliDbMigrateDev,
  )
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspMigrateSnapshotTest :: SnapshotTest
waspMigrateSnapshotTest =
  makeSnapshotTest "wasp-migrate" $ do
    createWaspProject minimalStarterTemplate
    inWaspProjectDir $ do
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
