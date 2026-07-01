module Wasp.Generator.ServerGenerator.Start
  ( startServer,
  )
where

import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Generator.Common (GeneratedAppDir)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import Wasp.Generator.ServerGenerator.ServerProcessSupervisor
  ( ServerProcessSupervisor,
    runServerProcessSupervisor,
  )
import qualified Wasp.Job as J

startServer :: Path' Abs (Dir GeneratedAppDir) -> ServerProcessSupervisor -> J.Job
startServer generatedAppDir supervisor =
  runServerProcessSupervisor serverDir supervisor
  where
    serverDir = generatedAppDir </> Common.serverRootDirInGeneratedAppDir
