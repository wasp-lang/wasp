module Wasp.Project.Studio
  ( startStudio,
  )
where

import System.Exit (ExitCode (..))
import qualified System.Process as P
import Wasp.NodePackageFFI (Package (WaspStudioPackage), getPackageProcessOptions)

startStudio ::
  -- | Path to the data JSON file.
  FilePath ->
  -- | All arguments from the Wasp CLI.
  IO (Either String ())
startStudio pathToDataFile = do
  let startStudioArgs = ["--data-file", pathToDataFile]

  cp <- getPackageProcessOptions WaspStudioPackage startStudioArgs
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
  case exitCode of
    ExitSuccess -> return $ Right ()
    ExitFailure code -> return $ Left $ "Studio command failed with exit code: " ++ show code
