module Generator.WebAppGenerator.Setup
  ( setupWebApp,
  )
where

import Generator.Common (ProjectRootDir)
import qualified Generator.Job as J
import Generator.Job.Process (runNodeCommandAsJob)
import qualified Generator.WebAppGenerator.Common as Common
import StrongPath (Abs, Dir, Path', (</>))

setupWebApp :: Path' Abs (Dir ProjectRootDir) -> J.Job
setupWebApp projectDir = do
  let webAppDir = projectDir </> Common.webAppRootDirInProjectRootDir
  runNodeCommandAsJob webAppDir "npm" ["install"] J.WebApp
