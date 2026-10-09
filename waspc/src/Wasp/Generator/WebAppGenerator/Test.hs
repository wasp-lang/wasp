module Wasp.Generator.WebAppGenerator.Test
  ( testWebApp,
  )
where

import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Exit (ExitCode)
import System.Process (CreateProcess (..), proc)
import Wasp.Env (getEnvVars, inheritEnvWith)
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig)
import qualified Wasp.Job as J
import Wasp.Project.Common (WaspProjectDir)

testWebApp :: WebAppRunConfig -> [String] -> Path' Abs (Dir WaspProjectDir) -> J.Job ExitCode
testWebApp clientRunConfig args waspProjectDir = do
  J.fromProc
    =<< inheritEnvWith
      (getEnvVars clientRunConfig)
      (proc "npx" ("vitest" : args)) {cwd = Just $ SP.fromAbsDir waspProjectDir}
