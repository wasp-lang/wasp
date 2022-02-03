module Wasp.Generator.WebAppGenerator.NpmInstall
  ( npmInstallClient,
  )
where

import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.Process (runNodeCommandAsJob)
import qualified Wasp.Generator.WebAppGenerator.Common as Common

npmInstallClient :: Path' Abs (Dir ProjectRootDir) -> J.Job
npmInstallClient projectDir = do
  let webAppDir = projectDir </> Common.webAppRootDirInProjectRootDir
  runNodeCommandAsJob webAppDir "npm" ["install"] J.WebApp
