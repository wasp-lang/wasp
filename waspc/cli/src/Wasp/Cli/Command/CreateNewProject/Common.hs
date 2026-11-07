module Wasp.Cli.Command.CreateNewProject.Common
  ( throwProjectCreationError,
    TemplateOutputDir,
  )
where

import Control.Monad.Except (throwError)
import Wasp.Cli.Command (Command, CommandError (..))

data TemplateOutputDir

throwProjectCreationError :: String -> Command a
throwProjectCreationError = throwError . CommandError "Project creation failed"
