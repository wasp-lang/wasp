module Wasp.Generator.WebAppGenerator.Start
  ( startWebApp,
  )
where

import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path')
import System.Environment (getExecutablePath)
import System.Exit (ExitCode)
import qualified Wasp.Generator.NodeCommand as NodeCommand
import Wasp.Generator.WebAppGenerator.Common (waspExePathEnvVarName)
import qualified Wasp.Generator.WebAppGenerator.NodeCommands as WebAppNodeCommands
import Wasp.Generator.WebAppGenerator.RunConfig (makeEnvVars)
import qualified Wasp.Job as J
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.RunConfig (ProjectRunConfig)

startWebApp :: ProjectRunConfig -> Path' Abs (Dir WaspProjectDir) -> J.Job ExitCode
startWebApp projectRunConfig waspProjectDir = do
  waspExePath <- liftIO getExecutablePath
  J.fromProc
    . J.addEnvVarsOverride ((waspExePathEnvVarName, waspExePath) : makeEnvVars projectRunConfig)
    -- Vite only enables its keyboard shortcuts when its stdin is a TTY.
    . J.markInteractive
    $ NodeCommand.proc waspProjectDir WebAppNodeCommands.runWebAppDevServer
