module Wasp.Cli.Command.Reset
  ( reset,
  )
where

import StrongPath (Dir, Path')
import qualified StrongPath as SP
import StrongPath.Types (Abs)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Common (deleteDirectoryIfExists, deleteDotWaspDirIfExists)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Project.Common (WaspProjectDir, nodeModulesDirInWaspProjectDir)

reset :: Command ()
reset = do
  InWaspProject waspProjectDir <- require
  deleteDotWaspDirIfExists waspProjectDir
  deleteNodeModulesDirIfExists waspProjectDir

deleteNodeModulesDirIfExists :: Path' Abs (Dir WaspProjectDir) -> Command ()
deleteNodeModulesDirIfExists waspProjectDir = deleteDirectoryIfExists nodeModulesDir
  where
    nodeModulesDir = waspProjectDir SP.</> nodeModulesDirInWaspProjectDir
