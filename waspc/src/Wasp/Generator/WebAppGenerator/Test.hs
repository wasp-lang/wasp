module Wasp.Generator.WebAppGenerator.Test
  ( testWebApp,
  )
where

import StrongPath (Abs, Dir, Path')
import qualified Wasp.Job as J
import qualified Wasp.Job.Node as Node
import Wasp.Project.Common (WaspProjectDir)

testWebApp :: [String] -> Path' Abs (Dir WaspProjectDir) -> J.Job
testWebApp args waspProjectDir =
  Node.makeJob waspProjectDir "npx" ("vitest" : args) J.WebApp
