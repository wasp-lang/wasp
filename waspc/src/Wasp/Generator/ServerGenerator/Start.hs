module Wasp.Generator.ServerGenerator.Start
  ( startServer,
  )
where

import StrongPath (Abs, Dir, Path', (</>))
import qualified StrongPath as SP
import System.Exit (ExitCode)
import System.Process (CreateProcess (..), proc)
import Wasp.Env (getEnvVars, inheritEnvWith)
import Wasp.Generator.Common (GeneratedAppDir)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig (..))
import qualified Wasp.Job as J

startServer :: ServerRunConfig -> Path' Abs (Dir GeneratedAppDir) -> J.Job e ExitCode
startServer serverRunConfig generatedAppDir = do
  let serverDir = SP.fromAbsDir $ generatedAppDir </> Common.serverRootDirInGeneratedAppDir

  J.fromInteractiveProc
    =<< inheritEnvWith
      (getEnvVars serverRunConfig)
      (proc "npm" ["run", "watch"]) {cwd = Just serverDir}
