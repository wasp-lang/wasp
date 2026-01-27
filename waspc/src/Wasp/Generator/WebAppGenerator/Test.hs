module Wasp.Generator.WebAppGenerator.Test
  ( testWebApp,
  )
where

import StrongPath (Abs, Dir, Path')
import qualified Wasp.Job as J
import Wasp.Job.Process (runNodeCommandAsJob)
import Wasp.Project.Common (WaspProjectDir)

testWebApp :: [String] -> Path' Abs (Dir WaspProjectDir) -> J.Job
testWebApp args waspProjectDir = do
  runNodeCommandAsJob waspProjectDir "npx" ("vitest" : args) J.WebApp
