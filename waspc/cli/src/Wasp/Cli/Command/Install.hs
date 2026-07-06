module Wasp.Cli.Command.Install
  ( install,
    installIO,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path')
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), ValidNodeAndNpm (ValidNodeAndNpm), require)
import Wasp.Project.Common (WaspProjectDir)
import qualified Wasp.Project.Module as ProjectModule

-- | Standalone `wasp install` command: copies @wasp.sh/spec and runs npm install.
install :: Command ()
install = do
  ValidNodeAndNpm <- require
  InWaspProject waspProjectDir <- require
  liftIO (installIO waspProjectDir)
    >>= either
      (throwError . CommandError "Couldn't install npm dependencies")
      return

installIO :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
installIO = ProjectModule.installWaspDependenciesIO
