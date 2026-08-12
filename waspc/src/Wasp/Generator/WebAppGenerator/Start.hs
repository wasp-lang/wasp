module Wasp.Generator.WebAppGenerator.Start
  ( startWebApp,
  )
where

import StrongPath (Abs, Dir, Path')
import Wasp.Generator.WebAppGenerator.RunConfig (ClientRunConfig (..))
import qualified Wasp.Job as J
import Wasp.Job.Process (runNodeCommandAsJobWithExtraEnv)
import Wasp.Project.Common (WaspProjectDir)

startWebApp :: ClientRunConfig -> Path' Abs (Dir WaspProjectDir) -> J.Job
startWebApp clientRunConfig waspProjectDir = do
  runNodeCommandAsJobWithExtraEnv clientRunConfig.envVars waspProjectDir "npx" ["vite"] J.WebApp
