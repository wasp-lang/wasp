module Wasp.Generator.WebAppGenerator.Setup
  ( installNpmDependencies,
  )
where

import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.WebAppGenerator.Common as Common
import qualified Wasp.Job as J
import Wasp.Job.Process (runNodeCommandAsJob)
import Wasp.Node.Commands (npmCmd)

installNpmDependencies :: Path' Abs (Dir ProjectRootDir) -> J.Job
installNpmDependencies projectDir = do
  let webAppDir = projectDir </> Common.webAppRootDirInProjectRootDir
  runNodeCommandAsJob webAppDir npmCmd ["install"] J.WebApp
