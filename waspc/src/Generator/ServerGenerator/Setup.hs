module Generator.ServerGenerator.Setup
  ( setupServer,
  )
where

import Generator.Common (ProjectRootDir)
import qualified Generator.Job as J
import Generator.Job.Process (runNodeCommandAsJob)
import qualified Generator.ServerGenerator.Common as Common
import StrongPath (Abs, Dir, Path, (</>))
import qualified System.Info

setupServer :: Path Abs (Dir ProjectRootDir) -> J.Job
setupServer projectDir = do
  let serverDir = projectDir </> Common.serverRootDirInProjectRootDir

  let npmInstallServerCmd = ["npm", "install"]
  let scriptArgs =
        if System.Info.os == "darwin"
          then -- NOTE: On MacOS, command that `script` should execute is treated as multiple arguments.
            ["-Fq", "/dev/null"] ++ npmInstallServerCmd
          else -- NOTE: On Linux, command that `script` should execute is treated as one argument.
            ["-feqc", unwords npmInstallServerCmd, "/dev/null"]

  runNodeCommandAsJob serverDir "script" scriptArgs J.Server
