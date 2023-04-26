module Wasp.Cli.Command.CreateNewProject.Common where

import Control.Monad.Except (throwError)
import Wasp.Cli.Command (Command, CommandError (..))

throwProjectCreationError :: String -> Command a
throwProjectCreationError = throwError . CommandError "Project creation failed"

throwInvalidTemplateNameUsedError :: Command a
throwInvalidTemplateNameUsedError = throwProjectCreationError "Are you sure that the template exists? 🤔 Check the list of templates here: https://github.com/wasp-lang/starters"
