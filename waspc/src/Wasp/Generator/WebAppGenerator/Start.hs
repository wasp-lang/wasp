module Wasp.Generator.WebAppGenerator.Start
  ( startWebApp,
  )
where

import StrongPath (Abs, Dir, Path')
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Job as J
import Wasp.Job.Process (runNodeCommandAsJob)
import Wasp.Project.Common (WaspProjectDir)

startWebApp :: Path' Abs (Dir WaspProjectDir) -> Path' Abs (Dir ProjectRootDir) -> J.Job
startWebApp waspProjectDir _projectDir = do
  -- Run vite from the wasp project root (where vite.config.ts lives)
  -- since the entry point is now in user-land
  runNodeCommandAsJob waspProjectDir "npx" ["vite"] J.WebApp
