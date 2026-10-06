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
import qualified Wasp.Job.Node as Node
import qualified Wasp.Job.Process as JobProcess

startServer :: ServerRunConfig -> Path' Abs (Dir GeneratedAppDir) -> J.Job ()
startServer serverRunConfig generatedAppDir = do
  let serverDir = generatedAppDir </> Common.serverRootDirInGeneratedAppDir
  J.withKind J.Server $
    JobProcess.run_ . JobProcess.interactive
      =<< Node.command
        (getEnvVars serverRunConfig)
        serverDir
        "npm"
        ["run", "watch"]
