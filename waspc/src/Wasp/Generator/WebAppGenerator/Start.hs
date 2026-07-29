module Wasp.Generator.WebAppGenerator.Start
  ( startWebApp,
  )
where

import StrongPath (Abs, Dir, Path')
import Wasp.Env (EnvVar)
import qualified Wasp.Job as J
import Wasp.Job.Process (runNodeCommandAsJobWithExtraEnv)
import Wasp.Project.Common (WaspProjectDir)

startWebApp :: [EnvVar] -> Path' Abs (Dir WaspProjectDir) -> J.Job
startWebApp waspEnvVars waspProjectDir = do
  runNodeCommandAsJobWithExtraEnv waspEnvVars waspProjectDir "npx" ["vite"] J.WebApp
