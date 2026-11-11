module Wasp.Generator.ServerGenerator.TaskRunner.Jobs
  ( runTask,
  )
where

import StrongPath (Abs, Dir, Path', (</>))
import System.Exit (ExitCode)
import Wasp.Env (EnvVar)
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.NodeCommand (NodeCommand (..))
import qualified Wasp.Generator.NodeCommand as NodeCommand
import Wasp.Generator.ServerGenerator.Common (serverRootDirInGeneratedAppDir)
import qualified Wasp.Generator.ServerGenerator.NodeCommands as ServerNodeCommands
import Wasp.Generator.ServerGenerator.TaskRunner (TaskRunner)
import qualified Wasp.Job as J
import Wasp.Project.Common (waspProjectDirFromGeneratedAppDir)

-- | Type-checks and bundles the server's task runner, then runs the task with
-- the given name, passing it the given arguments.
runTask :: TaskRunner -> [EnvVar] -> Path' Abs (Dir GeneratedAppDir) -> String -> [String] -> J.Job ExitCode
runTask runner envVars generatedAppDir taskName taskArgs =
  runInServerDir ServerNodeCommands.typeCheckServer
    `J.andThen` runInServerDir (ServerNodeCommands.bundleTaskRunner runner)
    `J.andThen` runRunner
  where
    runInServerDir nodeCommand =
      J.fromProc
        . J.addEnvVarsOverride envVars
        $ NodeCommand.proc serverDir nodeCommand
    runRunner =
      J.fromProc
        . J.addEnvVarsOverride envVars
        -- The task can ask for input.
        . J.markInteractive
        $ NodeCommand.proc waspProjectDir (appendArgs (taskName : taskArgs) $ ServerNodeCommands.runTaskRunner runner)
    appendArgs extraArgs (NodeCommand executable args) = NodeCommand executable (args ++ extraArgs)
    serverDir = generatedAppDir </> serverRootDirInGeneratedAppDir
    waspProjectDir = generatedAppDir </> waspProjectDirFromGeneratedAppDir
