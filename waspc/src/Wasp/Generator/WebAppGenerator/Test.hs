module Wasp.Generator.WebAppGenerator.Test
  ( testWebApp,
  )
where

import StrongPath (Abs, Dir, Path', relfile, (</>))
import qualified StrongPath as SP
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.Process (runNodeCommandAsJob)
import Wasp.Generator.WebAppGenerator.Common (webAppRootDirInProjectRootDir)
import Wasp.Project.Common (WaspProjectDir, dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir)

testWebApp :: [String] -> Path' Abs (Dir WaspProjectDir) -> J.Job
testWebApp args projectDir = do
  runNodeCommandAsJob projectDir "npx" (vitestCommand ++ args) J.WebApp
  where
    vitestCommand = ["vitest", "--config", SP.fromRelFile viteConfigPath]
    viteConfigPath =
      dotWaspDirInWaspProjectDir
        </> generatedCodeDirInDotWaspDir
        </> webAppRootDirInProjectRootDir
        </> [relfile|vite.config.ts|]
