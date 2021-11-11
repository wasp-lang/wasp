module Wasp.Generator.WebAppGenerator.Setup
  ( setupWebApp,
  )
where

import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.Process (runNodeCommandAsJob)
import qualified Wasp.Generator.WebAppGenerator.Common as Common

setupWebApp :: Path' Abs (Dir ProjectRootDir) -> J.Job
setupWebApp projectDir = do
  let webAppDir = projectDir </> Common.webAppRootDirInProjectRootDir
  runNodeCommandAsJob webAppDir "npm" ["install"] J.WebApp
