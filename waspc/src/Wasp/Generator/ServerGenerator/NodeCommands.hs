module Wasp.Generator.ServerGenerator.NodeCommands
  ( typeCheckServer,
    bundleServer,
    bundleTaskRunner,
    runServer,
    runTaskRunner,
    loadDotEnvFileInServerRootDir,
    LoadDotEnvFile,
  )
where

import StrongPath (Dir, File, File', Path', Rel, relfile, (</>))
import qualified StrongPath as SP
import Wasp.Generator.Common (ServerRootDir)
import Wasp.Generator.NodeCommand (NodeCommand (..))
import Wasp.Generator.ServerGenerator.Common (serverRootDirInGeneratedAppDir)
import Wasp.Generator.ServerGenerator.TaskRunner (TaskRunner, taskRunnerBundleInServerRootDir, taskRunnerRollupConfigInServerRootDir)
import Wasp.Project.Common (WaspProjectDir, generatedAppDirInWaspProjectDir)

-- | Through its project reference, `tsc --build` type-checks all of the user's
-- source files, client ones included, so their type errors also fail the
-- bundle.
typeCheckServer :: NodeCommand ServerRootDir
typeCheckServer = NodeCommand "tsc" ["--build"]

bundleServer :: NodeCommand ServerRootDir
bundleServer = NodeCommand "rollup" ["--config", "--silent"]

bundleTaskRunner :: TaskRunner -> NodeCommand ServerRootDir
bundleTaskRunner runner =
  NodeCommand "rollup" ["--config", SP.fromRelFile $ taskRunnerRollupConfigInServerRootDir runner, "--silent"]

runServer :: NodeCommand WaspProjectDir
runServer = nodeServerScript [relfile|bundle/server.js|]

-- | Extra arguments (the task name and its arguments) go after the command,
-- e.g. as npm appends them to an npm script.
runTaskRunner :: TaskRunner -> NodeCommand WaspProjectDir
runTaskRunner = nodeServerScript . taskRunnerBundleInServerRootDir

-- | The server runs with the Wasp project dir as its working directory, so that
-- relative paths in the user's code resolve from where the user expects them to.
--
-- Preloads 'loadDotEnvFileInServerRootDir' so the server gets its `.env` vars.
nodeServerScript :: Path' (Rel ServerRootDir) File' -> NodeCommand WaspProjectDir
nodeServerScript scriptInServerDir =
  NodeCommand
    "node"
    [ "--enable-source-maps",
      "-r",
      "./" <> SP.fromRelFile (serverDirInWaspProjectDir </> loadDotEnvFileInServerRootDir),
      SP.fromRelFile (serverDirInWaspProjectDir </> scriptInServerDir)
    ]
  where
    serverDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir ServerRootDir) =
      generatedAppDirInWaspProjectDir </> serverRootDirInGeneratedAppDir

-- | A script that loads the server's @.env@ file into @process.env@.
-- 'nodeServerScript' preloads it with @node -r@.
data LoadDotEnvFile

loadDotEnvFileInServerRootDir :: Path' (Rel ServerRootDir) (File LoadDotEnvFile)
loadDotEnvFileInServerRootDir = [relfile|loadDotEnv.cjs|]
