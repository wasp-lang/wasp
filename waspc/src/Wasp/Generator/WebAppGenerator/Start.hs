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
import qualified Wasp.Job.Subprocess as Subprocess
import Wasp.Project.Common (WaspProjectDir)

startWebApp :: WebAppRunConfig -> Path' Abs (Dir WaspProjectDir) -> Job.Job
startWebApp webAppRunConfig waspProjectDir =
  Job.makeJob Job.WebApp $ do
    createProcess <- liftIO $ Node.makeCreateProcess (getEnvVars webAppRunConfig) waspProjectDir "npx" ["vite"]
    subprocess <- Subprocess.spawn createProcess
    exitCode <- liftIO $ Subprocess.wait subprocess
    Job.requireExitSuccess exitCode
