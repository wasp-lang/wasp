module Wasp.Generator.ServerGenerator.Start
  ( startServer,
  )
where

import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Env (EnvVar)
import Wasp.Generator.Common (GeneratedAppDir)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import qualified Wasp.Job as J
import Wasp.Job.Process (runNodeCommandAsJobWithExtraEnv)

startServer :: [EnvVar] -> Path' Abs (Dir GeneratedAppDir) -> J.Job
startServer waspEnvVars generatedAppDir = do
  let serverDir = generatedAppDir </> Common.serverRootDirInGeneratedAppDir
  runNodeCommandAsJobWithExtraEnv waspEnvVars serverDir "npm" ["run", "watch"] J.Server
