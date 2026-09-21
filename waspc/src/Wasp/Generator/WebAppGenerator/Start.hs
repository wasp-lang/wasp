module Wasp.Generator.WebAppGenerator.Start
  ( startWebApp,
  )
where

import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path')
import Wasp.Env (getEnvVars)
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig (..))
import qualified Wasp.Job as Job
import qualified Wasp.Job.Node as Node
import qualified Wasp.Job.Process as JobProcess
import Wasp.Project.Common (WaspProjectDir)

startWebApp :: WebAppRunConfig -> Path' Abs (Dir WaspProjectDir) -> Job.Job ()
startWebApp webAppRunConfig waspProjectDir = do
  subprocess <- Node.spawn (getEnvVars webAppRunConfig) waspProjectDir "npx" ["vite"]
  exitCode <- liftIO $ JobProcess.wait subprocess
  Job.requireExitSuccess exitCode
