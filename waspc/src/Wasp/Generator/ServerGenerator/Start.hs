module Wasp.Generator.ServerGenerator.Start
  ( startServer,
  )
where

import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.Process (runNodeCommandAsJob)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import StrongPath (Abs, Dir, Path', (</>))

startServer :: Path' Abs (Dir ProjectRootDir) -> J.Job
startServer projectDir = do
  let serverDir = projectDir </> Common.serverRootDirInProjectRootDir
  runNodeCommandAsJob serverDir "npm" ["start"] J.Server
