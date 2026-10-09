module Wasp.Generator.WebAppGenerator.Start
  ( startWebApp,
  )
where

import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path')
import System.Exit (ExitCode)
import Wasp.Env (getEnvVars, setEnvVars)
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig (..))
import qualified Wasp.Job as J
import Wasp.Node.Bin (findNpmBin)
import Wasp.Project.Common (WaspProjectDir)

startWebApp :: WebAppRunConfig -> Path' Abs (Dir WaspProjectDir) -> J.Job ExitCode
startWebApp webAppRunConfig waspProjectDir = do
  Just vite <- liftIO $ findNpmBin waspProjectDir "vite"
  J.fromProc
    $ (`setEnvVars` getEnvVars webAppRunConfig)
    $ J.setCwd waspProjectDir
    $ J.proc vite []
