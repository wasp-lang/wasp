module Wasp.Generator.WebAppGenerator.Test
  ( testWebApp,
  )
where

import StrongPath (Abs, Dir, Path')
import Wasp.Env (getEnvVars)
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig)
import qualified Wasp.Job as J
import qualified Wasp.Job.Node as Node
import Wasp.Project.Common (WaspProjectDir)

testWebApp :: WebAppRunConfig -> [String] -> Path' Abs (Dir WaspProjectDir) -> J.Job
testWebApp clientRunConfig args waspProjectDir = do
  J.makeJob J.WebApp $
    Node.run
      (getEnvVars clientRunConfig)
      waspProjectDir
      "npx"
      ("vitest" : args)
