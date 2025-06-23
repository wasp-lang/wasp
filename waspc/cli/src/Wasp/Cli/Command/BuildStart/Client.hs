module Wasp.Cli.Command.BuildStart.Client
  ( buildClient,
    startClient,
  )
where

import Data.Function ((&))
import StrongPath ((</>))
import qualified StrongPath as SP
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.ServerGenerator.Common (defaultDevServerUrl)
import Wasp.Generator.WebAppGenerator.Common (defaultClientPort)
import qualified Wasp.Generator.WebAppGenerator.Common as Common
import qualified Wasp.Job as J
import Wasp.Job.Except (ExceptJob, toExceptJob)
import Wasp.Job.Process (runNodeCommandAsJob, runNodeCommandAsJobWithExtraEnv)

buildClient :: SP.Path' SP.Abs (SP.Dir ProjectRootDir) -> ExceptJob
buildClient buildDir =
  runNodeCommandAsJobWithExtraEnv
    [("REACT_APP_API_URL", defaultDevServerUrl)]
    webAppDir
    "npm"
    ["run", "build"]
    J.WebApp
    & toExceptJob (("Building the client failed with exit code: " <>) . show)
  where
    webAppDir = buildDir </> Common.webAppRootDirInProjectRootDir

startClient :: SP.Path' SP.Abs (SP.Dir ProjectRootDir) -> ExceptJob
startClient buildDir =
  runNodeCommandAsJob
    webAppDir
    "npm"
    [ "exec",
      "vite",
      "--",
      "preview", -- `preview` launches vite just as a webserver to the built files
      "--port",
      show defaultClientPort,
      "--strictPort" -- This will make it fail if the port is already in use
    ]
    J.WebApp
    & toExceptJob (("Serving the client failed with exit code: " <>) . show)
  where
    webAppDir = buildDir </> Common.webAppRootDirInProjectRootDir
