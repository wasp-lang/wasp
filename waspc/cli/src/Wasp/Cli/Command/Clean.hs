module Wasp.Cli.Command.Clean
  ( clean,
  )
where

import qualified StrongPath as SP
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Common (deleteDirectoryIfExistsVerbosely)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.ProjectLock (withProjectLock)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, nodeModulesDirInWaspProjectDir)
import Wasp.Util.Terminal (styleCode)

clean :: Command ()
clean = withProjectLock $ do
  InWaspProject waspProjectDir <- require

  let dotWaspDir = waspProjectDir SP.</> dotWaspDirInWaspProjectDir
  let nodeModulesDir = waspProjectDir SP.</> nodeModulesDirInWaspProjectDir

  deleteDirectoryIfExistsVerbosely nodeModulesDir
  -- We delete the .wasp dir last because it holds the project lock, so until
  -- it's gone, other Wasp commands can't start working on this project.
  deleteDirectoryIfExistsVerbosely dotWaspDir

  cliSendMessageC $
    Msg.Info $
      "\nRun " ++ styleCode "wasp install" ++ " to reinstall dependencies."
