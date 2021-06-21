module Generator.WebAppGenerator.Setup
  ( setupWebApp,
  )
where

import Generator.Common (ProjectRootDir)
import qualified Generator.Job as J
import Generator.Job.Process (runNodeCommandAsJob)
import qualified Generator.WebAppGenerator.Common as Common
import StrongPath (Abs, Dir, Path, (</>))
import qualified System.Info

setupWebApp :: Path Abs (Dir ProjectRootDir) -> J.Job
setupWebApp projectDir = do
  let webAppDir = projectDir </> Common.webAppRootDirInProjectRootDir

  let npmInstallWebAppCmd = ["npm", "install"]
  let scriptArgs =
        if System.Info.os == "darwin"
          then -- NOTE: On MacOS, command that `script` should execute is treated as multiple arguments.
            ["-Fq", "/dev/null"] ++ npmInstallWebAppCmd
          else -- NOTE: On Linux, command that `script` should execute is treated as one argument.
            ["-feqc", unwords npmInstallWebAppCmd, "/dev/null"]

  runNodeCommandAsJob webAppDir "script" scriptArgs J.WebApp
