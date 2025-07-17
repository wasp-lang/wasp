module Wasp.Generator.WebAppGenerator.Start
  ( startWebApp,
  )
where

import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.WebAppGenerator.Common as Common
import qualified Wasp.Job as J
import Wasp.Job.Process (runNodeCommandAsJob)

startWebApp :: Path' Abs (Dir ProjectRootDir) -> J.Job
startWebApp projectDir = do
  let webAppDir = projectDir </> Common.webAppRootDirInProjectRootDir
  -- We previously passed the absolute project root via the WASP_PROJECT_ROOT
  -- environment variable so the vendored DevToolsJson Vite plugin could
  -- resolve the correct path at runtime.  That plugin has now been replaced
  -- by the upstream `vite-plugin-devtools-json`, and the project root is
  -- injected directly into `vite.config.ts` during code-generation, so the
  -- extra environment variable is no longer required.
  runNodeCommandAsJob webAppDir "npm" ["start"] J.WebApp
