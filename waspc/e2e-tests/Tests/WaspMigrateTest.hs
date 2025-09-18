module Tests.WaspMigrateTest (waspMigrate) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( appendToPrismaFile,
    cdIntoCurrentProject,
    waspCliCompile,
    waspCliMigrate,
    waspCliNewMinimalStarter,
  )

waspMigrate :: GoldenTest
waspMigrate = do
  let taskModel =
        unlines
          [ "model Task {",
            "  id          Int     @id @default(autoincrement())",
            "  description String",
            "  isDone      Boolean @default(false)",
            "}"
          ]

  makeGoldenTest "waspMigrate" $
    sequence
      [ waspCliNewMinimalStarter,
        cdIntoCurrentProject,
        waspCliCompile,
        appendToPrismaFile taskModel,
        waspCliMigrate "foo"
      ]
