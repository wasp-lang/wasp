module Wasp.Cli.Command.Clean
  ( clean,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified StrongPath as SP
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Common (deleteDirectoryIfExistsVerbosely)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import qualified Wasp.Cli.ProjectLock as ProjectLock
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, nodeModulesDirInWaspProjectDir)
import Wasp.Util.Terminal (styleCode)
import qualified Wasp.Util.Terminal as Term

clean :: Command ()
clean = ProjectLock.acquireExclusive $ do
  InWaspProject waspProjectDir <- require

  let dotWaspDir = waspProjectDir SP.</> dotWaspDirInWaspProjectDir
  let nodeModulesDir = waspProjectDir SP.</> nodeModulesDirInWaspProjectDir

  deleteDirectoryIfExistsVerbosely nodeModulesDir
  -- We delete the .wasp dir last because it holds the project lock, so until
  -- it's gone, other Wasp commands can't start working on this project.
  deleteDirectoryIfExistsVerbosely dotWaspDir

  liftIO . putStrLn . Term.applyStyles [Term.Bold] $
    "! Run " ++ styleCode "wasp install" ++ " to reinstall dependencies"
