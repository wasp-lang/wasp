module Wasp.Generator.WebAppGenerator.Test
  ( testWebApp,
  )
where

import StrongPath (Abs, Dir, Path')
import Wasp.Generator.WebAppGenerator.RunConfig (makeEnvVars)
import qualified Wasp.Job as J
import Wasp.Job.Process (runNodeCommandAsJobWithExtraEnv)
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.RunConfig (ProjectRunConfig)

testWebApp :: ProjectRunConfig -> [String] -> Path' Abs (Dir WaspProjectDir) -> J.Job
testWebApp projectRunConfig args waspProjectDir = do
  runNodeCommandAsJobWithExtraEnv
    (makeEnvVars projectRunConfig)
    waspProjectDir
    "npx"
    ("vitest" : args)
    J.WebApp
