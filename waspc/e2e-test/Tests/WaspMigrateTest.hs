module Tests.WaspMigrateTest (waspMigrate) where

import ShellCommands
  ( MakeShellCommand,
    appendToWaspFile,
    cdIntoCurrentProject,
    combineMakeShellCommands,
    waspCliCompile,
    waspCliMigrate,
    waspCliNew,
  )

waspMigrate :: (String, MakeShellCommand)
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

  ("waspMigrate", makeShellCommand)
