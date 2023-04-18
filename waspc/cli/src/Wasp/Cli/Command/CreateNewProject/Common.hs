module Wasp.Cli.Command.CreateNewProject.Common where

import Control.Monad.Except (throwError)
import Wasp.Cli.Command (Command, CommandError (..))

throwProjectCreationError :: String -> Command a
throwProjectCreationError = throwError . CommandError "Project creation failed"
