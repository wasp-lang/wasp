module Wasp.Generator.NodeCommands
  ( typeCheckUserCode,
  )
where

import StrongPath (File, Path', Rel)
import qualified StrongPath as SP
import Wasp.Generator.NodeCommand (NodeCommand (..))
import Wasp.Project.Common (SrcTsConfigFile, WaspProjectDir)

typeCheckUserCode :: Path' (Rel WaspProjectDir) (File SrcTsConfigFile) -> NodeCommand WaspProjectDir
typeCheckUserCode srcTsConfigPath =
  NodeCommand "tsc" ["--project", SP.fromRelFile srcTsConfigPath, "--noEmit"]
