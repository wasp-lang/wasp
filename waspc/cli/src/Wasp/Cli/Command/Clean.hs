module Wasp.Cli.Command.Clean
  ( clean,
  )
where

import qualified StrongPath as SP
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Common (deleteDirectoryIfExistsVerbosely)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, nodeModulesDirInWaspProjectDir)

clean :: Command ()
clean = do
  InWaspProject waspProjectDir <- require

  let dotWaspDir = waspProjectDir SP.</> dotWaspDirInWaspProjectDir
  let nodeModulesDir = waspProjectDir SP.</> nodeModulesDirInWaspProjectDir

  deleteDirectoryIfExistsVerbosely dotWaspDir
  deleteDirectoryIfExistsVerbosely nodeModulesDir
