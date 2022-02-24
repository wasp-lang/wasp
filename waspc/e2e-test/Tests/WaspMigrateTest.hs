module Tests.WaspMigrateTest (waspMigrate) where

import GoldenTest (GoldenTest (GoldenTest, _goldenTestName, _makeShellCommand))
import ShellCommands
  ( appendToWaspFile,
    cdIntoCurrentProject,
    combineMakeShellCommands,
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

  let makeShellCommand =
        combineMakeShellCommands
          [ waspCliNew,
            cdIntoCurrentProject,
            waspCliCompile,
            appendToWaspFile entityDecl,
            waspCliMigrate "foo"
          ]

  GoldenTest {_goldenTestName = "waspMigrate", _makeShellCommand = makeShellCommand}
