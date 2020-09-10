module Generator.WebAppGenerator.Start
    ( startWebApp
    ) where

import           Generator.Common                 (ProjectRootDir)
import qualified Generator.Job                    as J
import           Generator.Job.Process            (runNodeCommandAsJob)
import qualified Generator.WebAppGenerator.Common as Common
import           StrongPath                       (Abs, Dir, Path, (</>))


startWebApp :: Path Abs (Dir ProjectRootDir) -> J.Job
startWebApp projectDir = do
    let webAppDir = projectDir </> Common.webAppRootDirInProjectRootDir
    runNodeCommandAsJob webAppDir "npm" ["start"] J.WebApp
