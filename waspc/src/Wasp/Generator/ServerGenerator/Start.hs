module Wasp.Generator.ServerGenerator.Start
  ( startServer,
  )
where

import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Env (getEnvVars)
import Wasp.Generator.Common (GeneratedAppDir)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig (..))
import qualified Wasp.Job as J
import Wasp.Job.Process (runNodeCommandAsJobWithExtraEnv)

startServer :: ServerRunConfig -> Path' Abs (Dir GeneratedAppDir) -> J.Job
startServer serverRunConfig generatedAppDir = do
  let serverDir = generatedAppDir </> Common.serverRootDirInGeneratedAppDir
  runNodeCommandAsJobWithExtraEnv
    (getEnvVars serverRunConfig)
    serverDir
    "npm"
    ["run", "watch"]
    J.Server
