module Tests.WaspBuildTest (waspBuild) where

import ShellCommands (MakeShellCommand, cdIntoCurrentProject, combineMakeShellCommands, setDbToPSQL, waspCliBuild, waspCliNew)

waspBuild :: (String, MakeShellCommand)
waspBuild = do
  let makeShellCommand =
        combineMakeShellCommands
          [ waspCliNew,
            cdIntoCurrentProject,
            setDbToPSQL,
            waspCliBuild
          ]

  ("waspBuild", makeShellCommand)
