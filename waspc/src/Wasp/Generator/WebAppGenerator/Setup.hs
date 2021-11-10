module Wasp.Generator.WebAppGenerator.Setup
  ( setupWebApp,
  )
where

import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.Process (runNodeCommandAsJob)
import qualified Wasp.Generator.WebAppGenerator.Common as Common
import StrongPath (Abs, Dir, Path', (</>))

setupWebApp :: Path' Abs (Dir ProjectRootDir) -> J.Job
setupWebApp projectDir = do
  let webAppDir = projectDir </> Common.webAppRootDirInProjectRootDir
  runNodeCommandAsJob webAppDir "npm" ["install"] J.WebApp
