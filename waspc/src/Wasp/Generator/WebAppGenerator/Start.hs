module Wasp.Generator.WebAppGenerator.Start
  ( startWebApp,
  )
where

import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Exit (ExitCode)
import System.Process (CreateProcess (..), proc)
import Wasp.Env (getEnvVars, inheritEnvWith)
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig (..))
import qualified Wasp.Job as J
import Wasp.Project.Common (WaspProjectDir)

startWebApp :: WebAppRunConfig -> Path' Abs (Dir WaspProjectDir) -> J.Job e ExitCode
startWebApp webAppRunConfig waspProjectDir = do
  J.fromProc
    =<< inheritEnvWith
      (getEnvVars webAppRunConfig)
      (proc "npx" ["vite"]) {cwd = Just $ SP.fromAbsDir waspProjectDir}
