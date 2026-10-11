module Wasp.Generator.WebAppGenerator.Start
  ( startWebApp,
  )
where

import StrongPath (Abs, Dir, Path')
import Wasp.Env (getEnvVars)
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig (..))
import qualified Wasp.Job as J
import Wasp.Job.Process (StdinMode, runNodeCommandAsJobWithExtraEnvAndStdinMode)
import Wasp.Project.Common (WaspProjectDir)

startWebApp :: StdinMode -> WebAppRunConfig -> Path' Abs (Dir WaspProjectDir) -> J.Job
startWebApp stdinMode webAppRunConfig waspProjectDir = do
  runNodeCommandAsJobWithExtraEnvAndStdinMode
    stdinMode
    (getEnvVars webAppRunConfig)
    waspProjectDir
    "npx"
    ["vite"]
    J.WebApp
