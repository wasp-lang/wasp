module Wasp.Generator.WebAppGenerator.Start
  ( startWebApp,
  )
where

import StrongPath (Abs, Dir, Path')
import Wasp.Generator.WebAppGenerator.RunConfig (makeEnvVars)
import qualified Wasp.Job as J
import Wasp.Job.Process (runNodeCommandAsJobWithExtraEnv)
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.RunConfig (ProjectRunConfig)

startWebApp :: ProjectRunConfig -> Path' Abs (Dir WaspProjectDir) -> J.Job
startWebApp projectRunConfig waspProjectDir = do
  runNodeCommandAsJobWithExtraEnv
    (makeEnvVars projectRunConfig)
    waspProjectDir
    "npx"
    ["vite"]
    J.WebApp
