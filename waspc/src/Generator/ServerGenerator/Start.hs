module Generator.ServerGenerator.Start
  ( startServer,
  )
where

import Generator.Common (ProjectRootDir)
import qualified Generator.Job as J
import Generator.Job.Process (runNodeCommandAsJob)
import qualified Generator.ServerGenerator.Common as Common
import StrongPath (Abs, Dir, Path, (</>))

startServer :: Path Abs (Dir ProjectRootDir) -> J.Job
startServer projectDir = do
  let serverDir = projectDir </> Common.serverRootDirInProjectRootDir
  runNodeCommandAsJob serverDir "npm" ["start"] J.Server
