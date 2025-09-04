module GoldenTests.WaspMigrate (waspMigrateGoldenTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( WaspStarter (Minimal),
    appendToPrismaFile,
    cdIntoCurrentProject,
    waspCliCompile,
    waspCliMigrate,
    waspCliNewStarter,
  )

waspMigrateGoldenTest :: GoldenTest
waspMigrateGoldenTest =
  makeGoldenTest "wasp-migrate" $
    sequence
      [ waspCliNewStarter Minimal,
        cdIntoCurrentProject,
        waspCliCompile,
        appendToPrismaFile taskModel,
        waspCliMigrate "foo"
      ]
  where
    taskModel =
      unlines
        [ "model Task {",
          "  id          Int     @id @default(autoincrement())",
          "  description String",
          "  isDone      Boolean @default(false)",
          "}"
        ]
