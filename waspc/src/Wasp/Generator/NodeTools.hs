-- | The Node tool commands Wasp runs on the generated app, each named after the
-- step it performs, and typed by the dir it must run from.
--
-- The Prisma CLI commands are in "Wasp.Generator.DbGenerator.Jobs".
module Wasp.Generator.NodeTools
  ( NodeCommand,
    proc,
    toShellCommand,

    -- * Server
    typeCheckServer,
    bundleServer,
    bundleDbSeed,
    runServer,
    runDbSeed,

    -- * Web app
    runWebAppDevServer,
    buildWebApp,
    previewWebApp,

    -- * SDK
    compileSdk,
    copySdkAssets,

    -- * User code
    typeCheckUserCode,
  )
where

import StrongPath (Abs, Dir, File, File', Path', Rel, relfile, (</>))
import qualified StrongPath as SP
import Wasp.Generator.Common (ServerRootDir)
import Wasp.Generator.SdkGenerator.Common (SdkRootDir)
import Wasp.Generator.ServerGenerator.Common (serverRootDirInGeneratedAppDir)
import qualified Wasp.Job as J
import Wasp.Project.Common (SrcTsConfigFile, WaspProjectDir, generatedAppDirInWaspProjectDir)

-- | An executable and its arguments, to run from a @dir@.
data NodeCommand dir = NodeCommand String [String]

-- | Runs the command from the dir, like an npm script would.
proc :: Path' Abs (Dir dir) -> NodeCommand dir -> J.Job J.CreateJobProcess
proc dir (NodeCommand executable args) = J.npmBinProc dir executable args

-- | The command as a shell command line, e.g. for an npm script.
toShellCommand :: NodeCommand dir -> String
toShellCommand (NodeCommand executable args) = unwords $ executable : args

-- | Through its project reference, `tsc --build` type-checks all of the user's
-- source files, client ones included, so their type errors also fail the
-- bundle.
typeCheckServer :: NodeCommand ServerRootDir
typeCheckServer = NodeCommand "tsc" ["--build"]

bundleServer :: NodeCommand ServerRootDir
bundleServer = NodeCommand "rollup" ["--config", "--silent"]

bundleDbSeed :: NodeCommand ServerRootDir
bundleDbSeed = NodeCommand "rollup" ["--config", "rollup.dbSeed.config.js", "--silent"]

runServer :: NodeCommand WaspProjectDir
runServer = nodeScriptFromWaspProjectDir [relfile|bundle/server.js|]

runDbSeed :: NodeCommand WaspProjectDir
runDbSeed = nodeScriptFromWaspProjectDir [relfile|bundle/dbSeed.js|]

-- | The server runs with the Wasp project dir as its working directory, so that
-- relative paths in the user's code resolve from where the user expects them to.
--
-- `loadDotEnv.cjs` loads the server's `.env` file.
nodeScriptFromWaspProjectDir :: Path' (Rel ServerRootDir) File' -> NodeCommand WaspProjectDir
nodeScriptFromWaspProjectDir scriptInServerDir =
  NodeCommand
    "node"
    [ "--enable-source-maps",
      "-r",
      "./" <> SP.fromRelFile (serverDirInWaspProjectDir </> [relfile|loadDotEnv.cjs|]),
      SP.fromRelFile (serverDirInWaspProjectDir </> scriptInServerDir)
    ]
  where
    serverDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir ServerRootDir) =
      generatedAppDirInWaspProjectDir </> serverRootDirInGeneratedAppDir

runWebAppDevServer :: NodeCommand WaspProjectDir
runWebAppDevServer = NodeCommand "vite" []

buildWebApp :: NodeCommand WaspProjectDir
buildWebApp = NodeCommand "vite" ["build"]

previewWebApp :: NodeCommand WaspProjectDir
previewWebApp =
  NodeCommand
    "vite"
    [ "preview", -- `preview` launches a static file server for the built client.
      "--strictPort" -- This will make it fail if the port is already in use.
    ]

compileSdk :: NodeCommand SdkRootDir
compileSdk = NodeCommand "tsc" []

copySdkAssets :: NodeCommand SdkRootDir
copySdkAssets = NodeCommand "node" ["./scripts/copy-assets.js"]

typeCheckUserCode :: Path' (Rel WaspProjectDir) (File SrcTsConfigFile) -> NodeCommand WaspProjectDir
typeCheckUserCode srcTsConfigPath =
  NodeCommand "tsc" ["--project", SP.fromRelFile srcTsConfigPath, "--noEmit"]
