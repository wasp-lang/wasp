module Wasp.Cli.Command.BuildStart.Client
  ( buildClient,
    startClient,
  )
where

import Data.Function ((&))
import StrongPath ((</>))
import qualified StrongPath as SP
import System.Exit (ExitCode (..))
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig)
import qualified Wasp.Cli.Command.BuildStart.Config as Config
import qualified Wasp.Generator.SdkGenerator.Client.VitePlugin.Common as ViteCommon
import Wasp.Generator.WebAppGenerator
  ( viteSsrBuildDirInWebAppDir,
    webAppRootDirInProjectRootDir,
  )
import qualified Wasp.Job as J
import Wasp.Job.Except (ExceptJob, toExceptJob)
import Wasp.Job.Process (runNodeCommandAsJob, runNodeCommandAsJobWithExtraEnv)
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir)

buildClient :: BuildStartConfig -> Bool -> ExceptJob
buildClient config ssrEnabled = toExceptJob errorMsg $ \jobChan -> do
  -- Run the standard client build first.
  buildResult <- runNodeCommandAsJobWithExtraEnv envVars projectDir "npx" ["vite", "build"] J.WebApp jobChan
  case buildResult of
    -- If not using SSR, just return the result of the standard build.
    _ | not ssrEnabled -> return buildResult
    -- If SSR is enabled but the standard build failed, return the failure.
    exitCode | exitCode /= ExitSuccess -> return exitCode
    -- If SSR is enabled and the standard build succeeded, also build the SSR entry point.
    _ -> runNodeCommandAsJobWithExtraEnv envVars projectDir "npx" ["vite", "build", "--ssr", ViteCommon.ssrEntryPointPath, "--outDir", ssrBuildOutDir] J.WebApp jobChan
  where
    errorMsg = ("Building the client failed with exit code: " <>) . show
    envVars = Config.clientEnvVars config
    projectDir = Config.projectDir config
    ssrBuildOutDir =
      SP.fromRelDir
        ( dotWaspDirInWaspProjectDir
            </> generatedCodeDirInDotWaspDir
            </> webAppRootDirInProjectRootDir
            </> viteSsrBuildDirInWebAppDir
        )

startClient :: BuildStartConfig -> Bool -> ExceptJob
startClient config ssrEnabled
  | ssrEnabled =
      runNodeCommandAsJob
        projectDir
        "node"
        [ ".wasp/out/web-app/server-ssr.mjs",
          "--port",
          port,
          "--strictPort"
        ]
        J.WebApp
        & toExceptJob (("Serving the client failed with exit code: " <>) . show)
  | otherwise =
      runNodeCommandAsJob
        projectDir
        "npx"
        [ "vite",
          "preview", -- `preview` launches a static file server for the built client.
          "--port",
          port,
          "--strictPort" -- This will make it fail if the port is already in use.
        ]
        J.WebApp
        & toExceptJob (("Serving the client failed with exit code: " <>) . show)
  where
    port = show $ Config.clientPort config

    projectDir = Config.projectDir config
