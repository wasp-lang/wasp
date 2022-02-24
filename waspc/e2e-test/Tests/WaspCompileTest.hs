module Tests.WaspCompileTest (waspCompile) where

import ShellCommands
  ( MakeShellCommand,
    cdIntoCurrentProject,
    combineMakeShellCommands,
    waspCliCompile,
    waspCliNew,
  )

waspCompile :: (String, MakeShellCommand)
waspCompile = do
  let makeShellCommand =
        combineMakeShellCommands
          [ waspCliNew,
            cdIntoCurrentProject,
            waspCliCompile
          ]

  ("waspCompile", makeShellCommand)
