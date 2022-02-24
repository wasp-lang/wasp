module Tests.WaspNewTest (waspNew) where

import ShellCommands (MakeShellCommand, waspCliNew)

waspNew :: (String, MakeShellCommand)
waspNew = ("waspNew", waspCliNew)
