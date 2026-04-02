module Wasp.Generator.ServerGenerator.Start
  ( startServer,
  )
where

import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Generator.Common (GeneratedAppDir)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import qualified Wasp.Job as J
import Wasp.Job.Process (runNodeCommandAsJob)

startServer :: Path' Abs (Dir GeneratedAppDir) -> J.Job
startServer generatedAppDir = do
  let serverDir = generatedAppDir </> Common.serverRootDirInGeneratedAppDir
  runNodeCommandAsJob serverDir "npm" ["run", "watch"] J.Server
