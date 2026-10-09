module Wasp.Generator.ServerGenerator.Start
  ( startServer,
  )
where

import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path', (</>))
import System.Exit (ExitCode)
import Wasp.Env (getEnvVars, setEnvVars)
import Wasp.Generator.Common (GeneratedAppDir)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig (..))
import qualified Wasp.Job as J
import Wasp.Node.Bin (findNpmBin)

startServer :: ServerRunConfig -> Path' Abs (Dir GeneratedAppDir) -> J.Job ExitCode
startServer serverRunConfig generatedAppDir = do
  let serverDir = generatedAppDir </> Common.serverRootDirInGeneratedAppDir
  Just nodemon <- liftIO $ findNpmBin serverDir "nodemon"

  J.fromProc
    $ (`setEnvVars` getEnvVars serverRunConfig)
    $ J.setCwd serverDir
    $ J.proc
      nodemon
      [ -- Configured in the server's `nodemon.json`.
      ]
