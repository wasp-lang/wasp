module Wasp.Cli.Command.BuildStart.Client
  ( buildClient,
    startClient,
  )
where

import qualified StrongPath as SP
import System.Exit (ExitCode)
import System.Process (CreateProcess (..), proc)
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig (..))
import Wasp.Env (getEnvVars)
import Wasp.Job.Fictional (inheritEnvWith)
import qualified Wasp.Job.Fictional as Job

buildClient :: BuildStartConfig -> Job.Job e ExitCode
buildClient config = do
  Job.fromProc
    =<< inheritEnvWith
      envVars
      (proc "npx" ["vite", "build"]) {cwd = Just projectDir}
  where
    envVars = getEnvVars config.clientRunConfig
    projectDir = SP.fromAbsDir config.projectDir

startClient :: BuildStartConfig -> Job.Job e ExitCode
startClient config = do
  Job.fromProc
    =<< inheritEnvWith
      envVars
      ( proc
          "npx"
          [ "vite",
            "preview", -- `preview` launches a static file server for the built client.
            "--strictPort" -- This will make it fail if the port is already in use.
          ]
      )
        { cwd = Just projectDir
        }
  where
    envVars = getEnvVars config.clientRunConfig
    projectDir = SP.fromAbsDir config.projectDir
