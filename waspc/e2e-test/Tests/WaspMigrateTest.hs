module Tests.WaspMigrateTest (waspMigrate) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( appendToWaspFile,
    cdIntoCurrentProject,
    waspCliCompile,
    waspCliMigrate,
    waspCliNew,
  )

waspMigrate :: GoldenTest
waspMigrate = do
  let entityDecl =
        "entity Task {=psl \n\
        \  id          Int     @id @default(autoincrement()) \n\
        \  description String \n\
        \  isDone      Boolean @default(false) \n\
        \ psl=} \n"

  makeGoldenTest "waspMigrate" $
    sequence
      [ waspCliNew,
        cdIntoCurrentProject,
        waspCliCompile,
        appendToWaspFile entityDecl,
        waspCliMigrate "foo"
      ]
