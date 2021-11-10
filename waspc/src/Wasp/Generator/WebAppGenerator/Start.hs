module Wasp.Generator.WebAppGenerator.Start
  ( startWebApp,
  )
where

import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.Process (runNodeCommandAsJob)
import qualified Wasp.Generator.WebAppGenerator.Common as Common
import StrongPath (Abs, Dir, Path', (</>))

startWebApp :: Path' Abs (Dir ProjectRootDir) -> J.Job
startWebApp projectDir = do
  let webAppDir = projectDir </> Common.webAppRootDirInProjectRootDir
  runNodeCommandAsJob webAppDir "npm" ["start"] J.WebApp
