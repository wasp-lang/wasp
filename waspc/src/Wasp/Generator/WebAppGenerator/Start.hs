module Wasp.Generator.WebAppGenerator.Start
  ( startWebApp,
  )
where

import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path')
import qualified Wasp.Job as Job
import qualified Wasp.Job.Node as Node
import qualified Wasp.Job.Subprocess as Subprocess
import Wasp.Project.Common (WaspProjectDir)

startWebApp :: Path' Abs (Dir WaspProjectDir) -> Job.Job
startWebApp waspProjectDir =
  Job.makeJob Job.WebApp $ do
    createProcess <- liftIO $ Node.makeCreateProcess waspProjectDir "npx" ["vite"]
    subprocess <- Subprocess.spawn createProcess
    exitCode <- liftIO $ Subprocess.wait subprocess
    Job.requireExitSuccess exitCode
