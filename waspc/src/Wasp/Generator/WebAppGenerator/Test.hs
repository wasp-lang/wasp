module Wasp.Generator.WebAppGenerator.Test
  ( testWebApp,
  )
where

import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.Process (runNodeCommandAsJob)
import qualified Wasp.Generator.WebAppGenerator.Common as Common

testWebApp :: [String] -> Path' Abs (Dir ProjectRootDir) -> J.Job
testWebApp args projectDir = do
  let webAppDir = projectDir </> Common.webAppRootDirInProjectRootDir
  runNodeCommandAsJob webAppDir "npx" ("vitest" : args) J.WebApp
