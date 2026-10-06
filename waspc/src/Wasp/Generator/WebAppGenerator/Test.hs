module Wasp.Generator.WebAppGenerator.Test
  ( testWebApp,
  )
where

import StrongPath (Abs, Dir, Path')
import System.Exit (ExitCode)
import Wasp.Env (getEnvVars)
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig)
import qualified Wasp.Job as J
import qualified Wasp.Job.Node as Node
import Wasp.Process (InputMode (InheritTerminal))
import Wasp.Project.Common (WaspProjectDir)

testWebApp :: WebAppRunConfig -> [String] -> Path' Abs (Dir WaspProjectDir) -> J.Job ExitCode
testWebApp clientRunConfig args waspProjectDir = do
  Node.run
    InheritTerminal
    (getEnvVars clientRunConfig)
    waspProjectDir
    "npx"
    ("vitest" : args)
