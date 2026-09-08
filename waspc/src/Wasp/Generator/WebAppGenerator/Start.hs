module Wasp.Generator.WebAppGenerator.Start
  ( startWebApp,
  )
where

import StrongPath (Abs, Dir, Path')
import Wasp.Env (getEnvVars)
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig (..))
import qualified Wasp.Job as J
import qualified Wasp.Job.Node as Node
import Wasp.Project.Common (WaspProjectDir)

startWebApp :: WebAppRunConfig -> Path' Abs (Dir WaspProjectDir) -> J.Job
startWebApp webAppRunConfig waspProjectDir = do
  Node.makeJobWithExtraEnv
    (getEnvVars webAppRunConfig)
    waspProjectDir
    "npx"
    ["vite"]
    J.WebApp
