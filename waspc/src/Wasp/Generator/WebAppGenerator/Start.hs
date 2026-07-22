module Wasp.Generator.WebAppGenerator.Start
  ( startWebApp,
  )
where

import StrongPath (Abs, Dir, Path')
import qualified Wasp.Job as J
import Wasp.Job.Node (makeNodeCommandProcessWithExtraEnv)
import qualified Wasp.Job.Process.LongRunning as LongRunning
import Wasp.Project.Common (WaspProjectDir)

startWebApp :: Path' Abs (Dir WaspProjectDir) -> J.Job
startWebApp waspProjectDir chan = do
  webAppProcess <- makeNodeCommandProcessWithExtraEnv [] waspProjectDir "npx" ["vite"]
  LongRunning.runAsJob webAppProcess J.WebApp chan
