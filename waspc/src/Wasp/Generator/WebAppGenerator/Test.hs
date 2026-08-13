module Wasp.Generator.WebAppGenerator.Test
  ( testWebApp,
  )
where

import StrongPath (Abs, Dir, Path')
import Wasp.Generator.WebAppGenerator.RunConfig (ClientRunConfig (..))
import qualified Wasp.Job as J
import Wasp.Job.Process (runNodeCommandAsJobWithExtraEnv)
import Wasp.Project.Common (WaspProjectDir)

testWebApp :: ClientRunConfig -> [String] -> Path' Abs (Dir WaspProjectDir) -> J.Job
testWebApp clientRunConfig args waspProjectDir = do
  runNodeCommandAsJobWithExtraEnv clientRunConfig.envVars waspProjectDir "npx" ("vitest" : args) J.WebApp
