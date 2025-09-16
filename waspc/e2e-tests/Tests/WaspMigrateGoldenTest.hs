module Tests.WaspMigrateGoldenTest (waspMigrateGoldenTest) where

import GoldenTest.Runner (GoldenTest, makeGoldenTest)
import GoldenTest.ShellCommands
  ( createGoldenTestWaspApp,
    withInGoldenTestWaspAppDir,
  )
import WaspApp.ShellCommands
  ( appendToPrismaFile,
    waspCliCompile,
    waspCliMigrate,
  )

waspMigrateGoldenTest :: GoldenTest
waspMigrateGoldenTest =
  makeGoldenTest
    "wasp-migrate"
    [ createGoldenTestWaspApp,
      withInGoldenTestWaspAppDir
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
