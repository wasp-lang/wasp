module Wasp.Generator.ServerGenerator.Start
  ( startServer,
  )
where

import StrongPath (Abs, Dir, Path', (</>))
import System.Exit (ExitCode)
import Wasp.Env (getEnvVars)
import Wasp.Generator.Common (GeneratedAppDir)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig (..))
import qualified Wasp.Job as J
import Wasp.Node.Bin (nodeBinProc)

startServer :: ServerRunConfig -> Path' Abs (Dir GeneratedAppDir) -> J.Job ExitCode
startServer serverRunConfig generatedAppDir = do
  let serverDir = generatedAppDir </> Common.serverRootDirInGeneratedAppDir

  J.fromProc
    =<< nodeBinProc
      (getEnvVars serverRunConfig)
      serverDir
      "nodemon" -- Configured in the server's `nodemon.json`.
      []
