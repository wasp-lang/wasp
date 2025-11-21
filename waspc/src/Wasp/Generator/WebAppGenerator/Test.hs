module Wasp.Generator.WebAppGenerator.Test
  ( testWebApp,
  )
where

import StrongPath (Abs, Dir, Path', relfile, (</>))
import qualified StrongPath as SP
import Wasp.Generator.WebAppGenerator.Common (webAppRootDirInProjectRootDir)
import qualified Wasp.Job as J
import Wasp.Job.Process (runNodeCommandAsJob)
import Wasp.Node.Executables (npxExec)
import Wasp.Project.Common (WaspProjectDir, dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir)

testWebApp :: [String] -> Path' Abs (Dir WaspProjectDir) -> J.Job
testWebApp args projectDir = do
  runNodeCommandAsJob projectDir npxExec (vitestCommand ++ args) J.WebApp
  where
    vitestCommand = ["vitest", "--config", SP.fromRelFile viteConfigPath]
    viteConfigPath =
      dotWaspDirInWaspProjectDir
        </> generatedCodeDirInDotWaspDir
        </> webAppRootDirInProjectRootDir
        </> [relfile|vite.config.ts|]
