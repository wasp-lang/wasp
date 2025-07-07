module Wasp.Generator.WebAppGenerator.Start
  ( startWebApp,
  )
where

import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.WebAppGenerator.Common as Common
import qualified Wasp.Job as J
import qualified StrongPath as SP
import Wasp.Job.Process (runNodeCommandAsJobWithExtraEnv)

startWebApp :: Path' Abs (Dir ProjectRootDir) -> J.Job
startWebApp projectDir = do
  let webAppDir = projectDir </> Common.webAppRootDirInProjectRootDir
      envVars = [("WASP_PROJECT_ROOT", SP.fromAbsDir projectDir)]
  runNodeCommandAsJobWithExtraEnv envVars webAppDir "npm" ["start"] J.WebApp
