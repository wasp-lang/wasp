module Wasp.Generator.WebAppGenerator.Start
  ( startWebApp,
  )
where

import StrongPath (Abs, Dir, Path')
import Wasp.Env (getEnvVars)
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig (..))
import qualified Wasp.Job as J
import qualified Wasp.Job.Node as Node
import qualified Wasp.Job.Process as JobProcess
import Wasp.Project.Common (WaspProjectDir)

startWebApp :: WebAppRunConfig -> Path' Abs (Dir WaspProjectDir) -> J.Job ()
startWebApp webAppRunConfig waspProjectDir =
  -- Wasp owns the shared terminal during `wasp start`, so Vite should not
  -- interpret keystrokes as its own shortcuts.
  J.withKind J.WebApp $
    JobProcess.run_
      =<< Node.command
        (getEnvVars webAppRunConfig)
        waspProjectDir
        "npx"
        ["vite"]
