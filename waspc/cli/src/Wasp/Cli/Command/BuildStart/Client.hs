module Wasp.Cli.Command.BuildStart.Client
  ( buildClient,
    startClient,
  )
where

import Control.Monad.IO.Class (liftIO)
import System.Exit (ExitCode)
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig (..))
import Wasp.Env (getEnvVars, setEnvVars)
import qualified Wasp.Job as Job
import Wasp.Node.Bin (findNpmBin)

buildClient :: BuildStartConfig -> Job.Job ExitCode
buildClient config = do
  Just vite <- liftIO $ findNpmBin config.projectDir "vite"
  Job.fromProc
    $ setEnvVars envVars
    $ Job.setCwd config.projectDir
    $ Job.proc
      vite
      ["build"]
  where
    envVars = getEnvVars config.clientRunConfig

startClient :: BuildStartConfig -> Job.Job ExitCode
startClient config = do
  Just vite <- liftIO $ findNpmBin config.projectDir "vite"
  Job.fromProc
    $ setEnvVars envVars
    $ Job.setCwd config.projectDir
    $ Job.proc
      vite
      [ "preview", -- `preview` launches a static file server for the built client.
        "--strictPort" -- This will make it fail if the port is already in use.
      ]
  where
    envVars = getEnvVars config.clientRunConfig
