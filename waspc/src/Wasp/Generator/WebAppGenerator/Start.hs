module Wasp.Generator.WebAppGenerator.Start
  ( startWebApp,
  )
where

import StrongPath (Abs, Dir, Path')
import qualified Wasp.Job as Job
import qualified Wasp.Job.Node as Node
import Wasp.Project.Common (WaspProjectDir)

startWebApp :: Path' Abs (Dir WaspProjectDir) -> Job.Job
startWebApp waspProjectDir =
  Node.makeJob waspProjectDir "npx" ["vite"] Job.WebApp
