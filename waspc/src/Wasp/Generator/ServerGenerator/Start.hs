module Wasp.Generator.ServerGenerator.Start
  ( startServer,
  )
where

import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Generator.Common (GeneratedAppDir)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import qualified Wasp.Job as Job
import qualified Wasp.Job.Node as Node

startServer :: Path' Abs (Dir GeneratedAppDir) -> Job.Job
startServer generatedAppDir =
  Node.makeJob serverDir "npm" ["run", "watch"] Job.Server
  where
    serverDir = generatedAppDir </> Common.serverRootDirInGeneratedAppDir
