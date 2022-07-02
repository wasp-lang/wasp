module Wasp.Generator.ServerGenerator.Setup
  ( installNpmDependencies,
  )
where

import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Generator.Common (ProjectRootDir, oSSpecificNpm)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.Process (runCommandThatRequiresNodeAsJob)
import qualified Wasp.Generator.ServerGenerator.Common as Common

installNpmDependencies :: Path' Abs (Dir ProjectRootDir) -> J.Job
installNpmDependencies projectDir = do
  let serverDir = projectDir </> Common.serverRootDirInProjectRootDir
  runCommandThatRequiresNodeAsJob serverDir oSSpecificNpm ["install"] J.Server
