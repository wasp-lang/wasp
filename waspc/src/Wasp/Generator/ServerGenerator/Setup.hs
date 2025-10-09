module Wasp.Generator.ServerGenerator.Setup
  ( installNpmDependencies,
  )
where

import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import qualified Wasp.Job as J
import Wasp.Job.Process (runNodeCommandAsJob)
import Wasp.Node.Commands (npmCmd)

installNpmDependencies :: Path' Abs (Dir ProjectRootDir) -> J.Job
installNpmDependencies projectDir = do
  let serverDir = projectDir </> Common.serverRootDirInProjectRootDir
  runNodeCommandAsJob serverDir npmCmd ["install"] J.Server
