module GoldenTests.WaspMigrateGoldenTest (waspMigrateGoldenTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( appendToPrismaFile,
    cdIntoGoldenTestProject,
    waspCliCompile,
    waspCliMigrate,
    waspCliNewMinimalStarter,
  )

waspMigrateGoldenTest :: GoldenTest
waspMigrateGoldenTest =
  makeGoldenTest "wasp-migrate" $
    sequence
      [ waspCliNewMinimalStarter "wasp-migrate",
        cdIntoGoldenTestProject $
          sequence
            [ waspCliCompile,
              appendToPrismaFile taskModel,
              waspCliMigrate "foo"
            ]
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
