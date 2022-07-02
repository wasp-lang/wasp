module Wasp.Generator.WebAppGenerator.Start
  ( startWebApp,
  )
where

import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Generator.Common (ProjectRootDir, buildNpmCmdWithArgs)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.Process (runCommandThatRequiresNodeAsJob)
import qualified Wasp.Generator.WebAppGenerator.Common as Common

startWebApp :: Path' Abs (Dir ProjectRootDir) -> J.Job
startWebApp projectDir = do
  let webAppDir = projectDir </> Common.webAppRootDirInProjectRootDir
  let (npmCmd, args) = buildNpmCmdWithArgs ["start"]
  runCommandThatRequiresNodeAsJob webAppDir npmCmd args J.WebApp
