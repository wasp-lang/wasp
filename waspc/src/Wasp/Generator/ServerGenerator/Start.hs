module Wasp.Generator.ServerGenerator.Start
  ( startServer,
  )
where

import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Generator.Common (ProjectRootDir, oSSpecificNpm)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.Process (runCommandThatRequiresNodeAsJob)
import qualified Wasp.Generator.ServerGenerator.Common as Common

startServer :: Path' Abs (Dir ProjectRootDir) -> J.Job
startServer projectDir = do
  let serverDir = projectDir </> Common.serverRootDirInProjectRootDir
  runCommandThatRequiresNodeAsJob serverDir oSSpecificNpm ["start"] J.Server
