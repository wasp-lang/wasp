module Wasp.Project.Studio
  ( startStudio,
    studioUrl,
  )
where

import qualified System.Process as P
import Wasp.NodePackageFFI (RunnablePackage (WaspStudioPackage), getPackageProcessOptions)
import Wasp.Util (checkExitSuccess)

-- | Where Wasp Studio serves its page. The Studio server always uses this
-- port.
studioUrl :: String
studioUrl = "http://localhost:4000"

-- | Serves the Wasp Studio page, which reads the app's spec from the server
-- that `wasp start` runs.
startStudio :: IO (Either String ())
startStudio = do
  cp <- getPackageProcessOptions WaspStudioPackage []
  -- Set up the process so that it:
  -- - Inherits handles from the waspc process (it will print and read from stdin/out/err)
  -- - Delegates Ctrl+C: when waspc receives Ctrl+C while this process is running,
  --   it will properly shut-down the child process.
  --   See https://hackage.haskell.org/package/process-1.6.17.0/docs/System-Process.html#g:4.
  let cpInheritHandles =
        cp
          { P.std_in = P.Inherit,
            P.std_out = P.Inherit,
            P.std_err = P.Inherit,
            P.delegate_ctlc = True
          }
  exitCode <- P.withCreateProcess cpInheritHandles $ \_ _ _ ph -> P.waitForProcess ph
  return $ checkExitSuccess "Studio command" exitCode
