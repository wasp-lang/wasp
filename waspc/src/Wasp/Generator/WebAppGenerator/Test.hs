module Wasp.Generator.WebAppGenerator.Test
  ( testWebApp,
  )
where

import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path')
import System.Exit (ExitCode)
import Wasp.Env (getEnvVars, setEnvVars)
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig)
import qualified Wasp.Job as J
import Wasp.Node.Bin (findNpmBin)
import Wasp.Project.Common (WaspProjectDir)

testWebApp :: WebAppRunConfig -> [String] -> Path' Abs (Dir WaspProjectDir) -> J.Job ExitCode
testWebApp clientRunConfig args waspProjectDir = do
  Just vitest <- liftIO $ findNpmBin waspProjectDir "vitest"
  J.fromProc
    $ setEnvVars (getEnvVars clientRunConfig)
    $ J.setCwd waspProjectDir
    $ J.proc vitest args
