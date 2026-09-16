module Wasp.Generator.WebAppGenerator.Start
  ( startWebApp,
  )
where

import StrongPath (Abs, Dir, Path')
import Wasp.Env (getEnvVars)
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig (..))
import qualified Wasp.Job as J
import Wasp.Job.Process (runNodeCommandAsJobWithExtraEnv)
import Wasp.Project.Common (WaspProjectDir)

startWebApp :: WebAppRunConfig -> Path' Abs (Dir WaspProjectDir) -> J.Job
startWebApp webAppRunConfig waspProjectDir = do
  runNodeCommandAsJobWithExtraEnv
    (getEnvVars webAppRunConfig)
    waspProjectDir
    "npx"
    ["vite"]
    J.WebApp
