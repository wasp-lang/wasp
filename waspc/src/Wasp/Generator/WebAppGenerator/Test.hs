module Wasp.Generator.WebAppGenerator.Test
  ( testWebApp,
  )
where

import StrongPath (Abs, Dir, Path')
import System.Exit (ExitCode)
import Wasp.Env (addEnvVarsOverride, getEnvVars)
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig)
import qualified Wasp.Job as J
import Wasp.Project.Common (WaspProjectDir)

testWebApp :: WebAppRunConfig -> [String] -> Path' Abs (Dir WaspProjectDir) -> J.Job ExitCode
testWebApp clientRunConfig args waspProjectDir =
  J.fromProc
    . addEnvVarsOverride (getEnvVars clientRunConfig)
    -- Vitest only runs in watch mode when its stdin is a TTY.
    . J.markInteractive
    =<< J.npmBinProc waspProjectDir "vitest" args
