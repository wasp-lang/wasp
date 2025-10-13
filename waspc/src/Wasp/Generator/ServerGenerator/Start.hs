module Wasp.Generator.ServerGenerator.Start
  ( startServer,
  )
where

import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import qualified Wasp.Job as J
import Wasp.Job.Process (runNodeCommandAsJob)
import Wasp.Node.Executables (npmExec)

startServer :: Path' Abs (Dir ProjectRootDir) -> J.Job
startServer projectDir = do
  let serverDir = projectDir </> Common.serverRootDirInProjectRootDir
  runNodeCommandAsJob serverDir npmExec ["run", "watch"] J.Server
