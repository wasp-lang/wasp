module Wasp.Project.Deployment
  ( loadUserDockerfileContents,
    deploy,
  )
where

import Control.Monad.Extra (whenMaybeM)
import Data.Text (Text)
import qualified Data.Text.IO as T.IO
import StrongPath (Abs, Dir, Path', relfile, toFilePath, (</>))
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import qualified System.Process as P
import Wasp.NodePackageFFI (Package (DeployPackage), getPackageProcessOptions)
import Wasp.Project.Common (WaspProjectDir)

loadUserDockerfileContents :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe Text)
loadUserDockerfileContents waspDir = do
  let dockerfileAbsPath = toFilePath $ waspDir </> [relfile|Dockerfile|]
  whenMaybeM (doesFileExist dockerfileAbsPath) $ T.IO.readFile dockerfileAbsPath

deploy ::
  -- | Path to wasp executable.
  FilePath ->
  Path' Abs (Dir WaspProjectDir) ->
  -- | All arguments from the Wasp CLI.
  [String] ->
  IO (Either String ())
deploy waspExe waspDir cmdArgs = do
  let deployScriptArgs = concat [cmdArgs, ["--wasp-exe", waspExe, "--wasp-project-dir", toFilePath waspDir]]
  cp <- getPackageProcessOptions DeployPackage deployScriptArgs
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
    ExitFailure code -> return $ Left $ "Deploy command failed with exit code: " ++ show code
