module Wasp.Generator.WebAppGenerator.Start
  ( startWebApp,
  )
where

import StrongPath (Abs, Dir, Path')
import qualified Wasp.Job as J
import Wasp.Job.Process (runNodeCommandAsJob)
import Wasp.Project.Common (WaspProjectDir)

startWebApp :: Path' Abs (Dir WaspProjectDir) -> J.Job
startWebApp waspProjectDir = do
  runNodeCommandAsJob waspProjectDir "npx" ["vite"] J.WebApp
