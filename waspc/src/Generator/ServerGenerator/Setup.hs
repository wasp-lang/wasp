module Generator.ServerGenerator.Setup
  ( setupServer,
  )
where

import Generator.Common (ProjectRootDir)
import qualified Generator.Job as J
import Generator.Job.Process (runNodeCommandAsJob)
import qualified Generator.ServerGenerator.Common as Common
import StrongPath (Abs, Dir, Path', (</>))

setupServer :: Path' Abs (Dir ProjectRootDir) -> J.Job
setupServer projectDir = do
  let serverDir = projectDir </> Common.serverRootDirInProjectRootDir
  runNodeCommandAsJob serverDir "npm" ["install"] J.Server
