module Wasp.Generator.WebAppGenerator.Start
  ( startWebApp,
  )
where

import StrongPath (Abs, Dir, Path')
import System.Exit (ExitCode)
import Wasp.Env (getEnvVars)
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig (..))
import qualified Wasp.Job as J
import qualified Wasp.Job.Node as Node
import Wasp.Process (InputMode (InheritTerminal))
import Wasp.Project.Common (WaspProjectDir)

startWebApp :: WebAppRunConfig -> Path' Abs (Dir WaspProjectDir) -> J.Job ExitCode
startWebApp webAppRunConfig waspProjectDir = do
  Node.run
    InheritTerminal
    (getEnvVars webAppRunConfig)
    waspProjectDir
    "npx"
    ["vite"]
