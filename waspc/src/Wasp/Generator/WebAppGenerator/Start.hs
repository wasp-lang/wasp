module Wasp.Generator.WebAppGenerator.Start
  ( startWebApp,
  )
where

import StrongPath (Abs, Dir, Path')
import System.Exit (ExitCode)
import Wasp.Env (getEnvVars, setEnvVars)
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig (..))
import qualified Wasp.Job as J
import Wasp.Project.Common (WaspProjectDir)

startWebApp :: WebAppRunConfig -> Path' Abs (Dir WaspProjectDir) -> J.Job ExitCode
startWebApp webAppRunConfig waspProjectDir = do
  J.fromProc
    $ setEnvVars (getEnvVars webAppRunConfig)
    $ J.setCwd waspProjectDir
    -- Vite only enables its keyboard shortcuts when its stdin is a TTY.
    $ J.markInteractive
    $ J.proc "npx" ["vite"]
