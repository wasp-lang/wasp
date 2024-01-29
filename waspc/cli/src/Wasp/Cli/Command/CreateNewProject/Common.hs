module Wasp.Cli.Command.CreateNewProject.Common
  ( throwProjectCreationError,
    throwInvalidTemplateNameUsedError,
    defaultWaspVersionBounds,
  )
where

import Control.Monad.Except (throwError)
import Wasp.Cli.Command (Command, CommandError (..))
import qualified Wasp.SemanticVersion as SV
import qualified Wasp.Version as WV

throwProjectCreationError :: String -> Command a
throwProjectCreationError = throwError . CommandError "Project creation failed"

throwInvalidTemplateNameUsedError :: Command a
throwInvalidTemplateNameUsedError =
  throwProjectCreationError $
    "Are you sure that the template exists?"
      <> " 🤔 Check the list of templates here: https://github.com/wasp-lang/starters"

defaultWaspVersionBounds :: String
defaultWaspVersionBounds = show (SV.backwardsCompatibleWith WV.waspVersion)
