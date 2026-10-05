module Wasp.Cli.Command.Compile
  ( compileCommand,
  )
where

import qualified Wasp.AppSpec as AS
import Wasp.Cli.Command (Command)
import Wasp.Cli.Compile (compile)
import qualified Wasp.Cli.ProjectLock as ProjectLock
import Wasp.Project (CompileWarning)

-- | Meant for the standalone `wasp compile` command: commands that hold the
-- project lock themselves should call 'compile' instead.
compileCommand :: Command ([CompileWarning], AS.AppSpec)
compileCommand = ProjectLock.acquireExclusive compile
