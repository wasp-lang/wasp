-- | The Node tools Wasp runs on the generated app, each named after the step it
-- performs. This is the place to look for what Wasp runs, and from where.
--
-- The Prisma CLI commands are in "Wasp.Generator.DbGenerator.Jobs".
module Wasp.Generator.NodeTools
  ( -- * Server
    bundleServer,
    runDevServer,
    bundleDbSeed,
    runDbSeed,
    serverBundleCommand,
    devServerStartCommand,

    -- * Web app
    runWebAppDevServer,
    buildWebApp,
    previewWebApp,

    -- * SDK
    buildSdk,

    -- * User code
    typeCheckUserCode,
  )
where

import StrongPath (Abs, Dir, File', Path', Rel, relfile, (</>))
import qualified StrongPath as SP
import System.Exit (ExitCode)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.Env (addEnvVarsOverride, getEnvVars)
import Wasp.Generator.Common (GeneratedAppDir, ServerRootDir)
import Wasp.Generator.SdkGenerator.Common (sdkRootDirInGeneratedAppDir)
import Wasp.Generator.ServerGenerator.Common (serverRootDirInGeneratedAppDir)
import Wasp.Generator.ServerGenerator.Db.Seed (dbSeedNameEnvVarName)
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig)
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig)
import qualified Wasp.Job as J
import Wasp.Project.Common
  ( WaspProjectDir,
    generatedAppDirInWaspProjectDir,
    waspProjectDirFromGeneratedAppComponentDir,
    waspProjectDirFromGeneratedAppDir,
  )

-- | Type-checks the server and bundles it into `bundle/server.js`.
bundleServer :: Path' Abs (Dir GeneratedAppDir) -> J.Job ExitCode
bundleServer generatedAppDir =
  runServerTool generatedAppDir serverTscProcess
    `J.andThen` runServerTool generatedAppDir serverRollupProcess

-- | Runs the server bundle made by 'bundleServer'.
runDevServer :: ServerRunConfig -> Path' Abs (Dir GeneratedAppDir) -> J.Job ExitCode
runDevServer serverRunConfig generatedAppDir =
  J.fromProc
    $ addEnvVarsOverride (("NODE_ENV", "development") : getEnvVars serverRunConfig)
    $ J.setCwd (generatedAppDir </> waspProjectDirFromGeneratedAppDir)
    $ uncurry J.proc devServerStartProcess

-- | Type-checks the server and bundles the db seed script into
-- `bundle/dbSeed.js`.
bundleDbSeed :: Path' Abs (Dir GeneratedAppDir) -> J.Job ExitCode
bundleDbSeed generatedAppDir =
  runServerTool generatedAppDir serverTscProcess
    `J.andThen` runServerTool generatedAppDir ("rollup", ["--config", "rollup.dbSeed.config.js", "--silent"])

-- | Runs the db seed with the given name from the bundle made by 'bundleDbSeed'.
runDbSeed :: ServerRunConfig -> Path' Abs (Dir GeneratedAppDir) -> String -> J.Job ExitCode
runDbSeed serverRunConfig generatedAppDir seedName =
  J.fromProc
    $ addEnvVarsOverride ((dbSeedNameEnvVarName, seedName) : getEnvVars serverRunConfig)
    $ J.setCwd (generatedAppDir </> waspProjectDirFromGeneratedAppDir)
    -- The user's seed function can ask for input.
    $ J.markInteractive
    $ uncurry J.proc dbSeedProcess

-- | The command for the server's `bundle` npm script, which the Dockerfile
-- runs. Runs from the server dir, like all npm scripts.
serverBundleCommand :: String
serverBundleCommand = processToCommand serverTscProcess <> " && " <> processToCommand serverRollupProcess

-- | The command for the server's `start` npm script, which `npm run
-- start-production` runs. Runs from the server dir, like all npm scripts.
devServerStartCommand :: String
devServerStartCommand =
  "cd " <> SP.fromRelDir waspProjectDirFromServerDir <> " && " <> processToCommand devServerStartProcess
  where
    waspProjectDirFromServerDir :: Path' (Rel ServerRootDir) (Dir WaspProjectDir) =
      waspProjectDirFromGeneratedAppComponentDir

-- | Through its project reference, `tsc --build` type-checks all of the user's
-- source files, client ones included, so their type errors also fail the
-- bundle.
serverTscProcess :: (String, [String])
serverTscProcess = ("tsc", ["--build"])

serverRollupProcess :: (String, [String])
serverRollupProcess = ("rollup", ["--config", "--silent"])

-- | Runs from the Wasp project dir.
devServerStartProcess :: (String, [String])
devServerStartProcess = nodeProcessFromWaspProjectDir [relfile|bundle/server.js|]

-- | Runs from the Wasp project dir.
dbSeedProcess :: (String, [String])
dbSeedProcess = nodeProcessFromWaspProjectDir [relfile|bundle/dbSeed.js|]

-- | The server runs with the Wasp project dir as its working directory, so that
-- relative paths in the user's code resolve from where the user expects them to.
--
-- `loadDotEnv.cjs` loads the server's `.env` file.
nodeProcessFromWaspProjectDir :: Path' (Rel ServerRootDir) File' -> (String, [String])
nodeProcessFromWaspProjectDir scriptInServerDir =
  ( "node",
    [ "--enable-source-maps",
      "-r",
      "./" <> SP.fromRelFile (serverDirInWaspProjectDir </> [relfile|loadDotEnv.cjs|]),
      SP.fromRelFile (serverDirInWaspProjectDir </> scriptInServerDir)
    ]
  )
  where
    serverDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir ServerRootDir) =
      generatedAppDirInWaspProjectDir </> serverRootDirInGeneratedAppDir

runServerTool :: Path' Abs (Dir GeneratedAppDir) -> (String, [String]) -> J.Job ExitCode
runServerTool generatedAppDir (binName, args) =
  J.fromProc =<< J.npmBinProc (generatedAppDir </> serverRootDirInGeneratedAppDir) binName args

processToCommand :: (String, [String]) -> String
processToCommand (executable, args) = unwords $ executable : args

-- | Runs the Vite dev server for the web app.
runWebAppDevServer :: WebAppRunConfig -> Path' Abs (Dir WaspProjectDir) -> J.Job ExitCode
runWebAppDevServer webAppRunConfig waspProjectDir =
  J.fromProc
    . addEnvVarsOverride (getEnvVars webAppRunConfig)
    -- Vite only enables its keyboard shortcuts when its stdin is a TTY.
    . J.markInteractive
    =<< J.npmBinProc waspProjectDir "vite" []

-- | Builds the web app for production.
buildWebApp :: WebAppRunConfig -> Path' Abs (Dir WaspProjectDir) -> J.Job ExitCode
buildWebApp webAppRunConfig waspProjectDir =
  J.fromProc
    . addEnvVarsOverride (getEnvVars webAppRunConfig)
    =<< J.npmBinProc waspProjectDir "vite" ["build"]

-- | Serves the web app built by 'buildWebApp'.
previewWebApp :: WebAppRunConfig -> Path' Abs (Dir WaspProjectDir) -> J.Job ExitCode
previewWebApp webAppRunConfig waspProjectDir =
  J.fromProc
    . addEnvVarsOverride (getEnvVars webAppRunConfig)
    =<< J.npmBinProc
      waspProjectDir
      "vite"
      [ "preview", -- `preview` launches a static file server for the built client.
        "--strictPort" -- This will make it fail if the port is already in use.
      ]

-- | Compiles the SDK and copies its non-TypeScript assets next to the output.
buildSdk :: Path' Abs (Dir GeneratedAppDir) -> J.Job ExitCode
buildSdk generatedAppDir =
  (J.fromProc =<< J.npmBinProc sdkRootDir "tsc" [])
    `J.andThen` J.fromProc (J.setCwd sdkRootDir $ J.proc "node" ["./scripts/copy-assets.js"])
  where
    sdkRootDir = generatedAppDir </> sdkRootDirInGeneratedAppDir

-- | Type-checks the user's source files, without emitting anything.
typeCheckUserCode :: AppSpec -> J.Job ExitCode
typeCheckUserCode spec =
  J.fromProc
    =<< J.npmBinProc
      (AS.waspProjectDir spec)
      "tsc"
      [ "--project",
        SP.fromRelFile $ AS.srcTsConfigPath spec,
        "--noEmit"
      ]
