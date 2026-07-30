module Wasp.Generator.WebAppGenerator.Test
  ( testWebApp,
  )
where

import StrongPath (Abs, Dir, Path')
import Wasp.Env (EnvVar)
import qualified Wasp.Job as J
import Wasp.Job.Process (runNodeCommandAsJobWithExtraEnv)
import Wasp.Project.Common (WaspProjectDir)

testWebApp :: [EnvVar] -> [String] -> Path' Abs (Dir WaspProjectDir) -> J.Job
testWebApp waspEnvVars args waspProjectDir = do
  runNodeCommandAsJobWithExtraEnv waspEnvVars waspProjectDir "npx" ("vitest" : args) J.WebApp
