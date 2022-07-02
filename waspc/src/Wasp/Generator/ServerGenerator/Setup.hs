module Wasp.Generator.ServerGenerator.Setup
  ( installNpmDependencies,
  )
where

import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Generator.Common (ProjectRootDir, buildNpmCmdWithArgs)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.Process (runCommandThatRequiresNodeAsJob)
import qualified Wasp.Generator.ServerGenerator.Common as Common

installNpmDependencies :: Path' Abs (Dir ProjectRootDir) -> J.Job
installNpmDependencies projectDir = do
  let serverDir = projectDir </> Common.serverRootDirInProjectRootDir
  let (npmCmd, args) = buildNpmCmdWithArgs ["install"]
  runCommandThatRequiresNodeAsJob serverDir npmCmd args J.Server
