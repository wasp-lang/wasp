module Wasp.Generator.ServerGenerator.Setup
  ( installNpmDependencies,
  )
where

import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Generator.Common (ProjectRootDir, buildNpmCmdWithArgs)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.Process (runNodeDependentCommandAsJob)
import qualified Wasp.Generator.ServerGenerator.Common as Common

installNpmDependencies :: Path' Abs (Dir ProjectRootDir) -> J.Job
installNpmDependencies projectDir = do
  let serverDir = projectDir </> Common.serverRootDirInProjectRootDir
  runNodeDependentCommandAsJob J.Server serverDir $ buildNpmCmdWithArgs ["install"]
