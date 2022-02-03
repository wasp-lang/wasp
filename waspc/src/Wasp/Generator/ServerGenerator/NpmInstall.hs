module Wasp.Generator.ServerGenerator.NpmInstall
  ( npmInstallServer,
  )
where

import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.Process (runNodeCommandAsJob)
import qualified Wasp.Generator.ServerGenerator.Common as Common

npmInstallServer :: Path' Abs (Dir ProjectRootDir) -> J.Job
npmInstallServer projectDir = do
  let serverDir = projectDir </> Common.serverRootDirInProjectRootDir
  runNodeCommandAsJob serverDir "npm" ["install"] J.Server
