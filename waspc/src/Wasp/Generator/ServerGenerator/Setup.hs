module Wasp.Generator.ServerGenerator.Setup
  ( setupServer,
  )
where

import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.Process (runNodeCommandAsJob)
import qualified Wasp.Generator.ServerGenerator.Common as Common

setupServer :: Path' Abs (Dir ProjectRootDir) -> J.Job
setupServer projectDir = do
  let serverDir = projectDir </> Common.serverRootDirInProjectRootDir
  runNodeCommandAsJob serverDir "npm" ["install"] J.Server
