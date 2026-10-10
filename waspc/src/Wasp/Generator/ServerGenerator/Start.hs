module Wasp.Generator.ServerGenerator.Start
  ( startServer,
  )
where

import StrongPath (Abs, Dir, Path', (</>))
import System.Exit (ExitCode)
import Wasp.Env (getEnvVars, setEnvVars)
import Wasp.Generator.Common (GeneratedAppDir)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig (..))
import qualified Wasp.Job as J

startServer :: ServerRunConfig -> Path' Abs (Dir GeneratedAppDir) -> J.Job ExitCode
startServer serverRunConfig generatedAppDir = do
  let serverDir = generatedAppDir </> Common.serverRootDirInGeneratedAppDir

  J.fromProc
    $ setEnvVars (getEnvVars serverRunConfig)
    $ J.setCwd serverDir
    $ J.proc "npm" ["run", "watch"]
